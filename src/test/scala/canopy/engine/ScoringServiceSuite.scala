package canopy.engine

import canopy.TestData.*
import canopy.cli.Io
import canopy.domain.{Label, Outcome, Verdict}
import canopy.rules.RuleSet
import cats.effect.IO
import cats.syntax.all.*
import munit.CatsEffectSuite

class ScoringServiceSuite extends CatsEffectSuite:
  private val service = ResourceFunFixture(
    cats.effect.Resource.eval(
      for
        model <- Io.loadModel(None)
        rules <- Io.loadRules(None)
        s <- ScoringService.create(model, rules)
      yield s
    )
  )

  service.test("an ordinary purchase is approved by the model") { s =>
    s.score(tx("t1", device = Some("dev-a"), ipCountry = Some("US"))).map { d =>
      assertEquals(d.verdict, Verdict.Approve)
      assertEquals(d.decidedBy, "model")
    }
  }

  service.test("a card-testing burst trips the rule and then the decline") { s =>
    val burst = (1 to 6).toList.map(i =>
      tx(
        s"ct$i",
        merchant = s"probe-merchant-$i",
        card = "victim",
        account = "acct-v",
        amount = "1.00",
        device = Some("fraud-dev"),
        ipCountry = Some("RO"),
        at = t0.plusMinutes(i.toLong)
      )
    )
    burst.traverse(s.score).map { ds =>
      assertEquals(ds.last.verdict, Verdict.Decline)
      assertEquals(ds.last.decidedBy, "rule:card_testing")
      assert(ds.last.tags.contains("card-testing"))
    }
  }

  service.test("retried transaction ids return the original decision and are not double counted") {
    s =>
      val t = tx("dup", card = "c-dup", account = "a-dup")
      for
        first <- s.score(t)
        again <- s.score(t.copy(amount = canopy.domain.Money(999999, t.amount.currency)))
        stats <- s.stats
        metrics <- s.metrics.snapshot
      yield
        assertEquals(again, first)
        assertEquals(stats.processed, 1L)
        assertEquals(metrics.duplicates, 1L)
  }

  service.test("dry runs do not change state") { s =>
    for
      _ <- s.preview(tx("p1"))
      _ <- s.preview(tx("p2"))
      stats <- s.stats
    yield assertEquals(stats.processed, 0L)
  }

  service.test("labels propagate through the graph to linked cards") { s =>
    val shared = (1 to 3).toList.map(i =>
      tx(
        s"r$i",
        card = s"ring-$i",
        account = s"ring-a$i",
        device = Some("ring-device"),
        at = t0.plusMinutes(i.toLong)
      )
    )
    for
      _ <- shared.traverse(s.score)
      _ <- List("ring-1", "ring-2").traverse(c =>
        s.label(Label("x", c, Outcome.Fraud, t0.plusHours(1)))
      )
      fv <- s.features(tx(
        "r4",
        card = "ring-4",
        account = "ring-a4",
        device = Some("ring-device"),
        at = t0.plusHours(2)
      ))
      d <- s.score(tx(
        "r4",
        card = "ring-4",
        account = "ring-a4",
        device = Some("ring-device"),
        at = t0.plusHours(2)
      ))
    yield
      assertEquals(fv.num("graph.fraud_cards"), Some(2.0))
      assertEquals(d.verdict, Verdict.Review)
      assertEquals(d.decidedBy, "rule:linked_to_fraud")
  }

  service.test("rules can be hot swapped and bad programs are rejected without effect") { s =>
    for
      bad <- s.replaceRules("rule r when nope then approve")
      before <- s.current
      good <-
        s.replaceRules("rule block_all priority 1 when amount_usd > 0 then decline reason \"test\"")
      d <- s.score(tx("hs1", card = "hs"))
    yield
      assert(bad.isLeft)
      assert(before.rules.size > 1)
      assertEquals(good.map(_.size), Right(1))
      assertEquals(d.decidedBy, "rule:block_all")
  }

  service.test("concurrent scoring keeps per-card velocity exact") { s =>
    val txs = (1 to 50).toList.map(i =>
      tx(s"c$i", card = "busy", account = "busy-a", at = t0.plusMinutes(i.toLong))
    )
    for
      _ <- txs.parTraverse(s.score)
      fv <- s.features(tx("probe", card = "busy", account = "busy-a", at = t0.plusMinutes(51)))
    yield assertEquals(fv.num("card.count_1h"), Some(51.0))
  }

  test("an empty rule set is valid") {
    assertEquals(RuleSet.compile("# nothing here\n").map(_.size), Right(0))
  }
