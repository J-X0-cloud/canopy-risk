package canopy.cli

import canopy.sim.SimConfig
import cats.effect.{ExitCode, IO}
import fs2.io.file.{Files, Path}
import munit.CatsEffectSuite

class CliSuite extends CatsEffectSuite:
  private val tmp = ResourceFunFixture(Files[IO].tempDirectory(None, "canopy-cli", None))

  private val small = SimConfig(
    seed = 5,
    cardholders = 120,
    days = 7,
    cardTestingAttacks = 6,
    takeovers = 6,
    fraudRings = 2
  )

  tmp.test("simulate, train from files, replay and backtest work end to end") { dir =>
    val events = (dir / "events.ndjson").toString
    val labels = (dir / "labels.ndjson").toString
    val model = (dir / "model" / "model.json").toString
    val decisions = (dir / "decisions.ndjson").toString
    for
      s <- Commands.simulate(small, events, labels)
      t <- Commands.train(Right((events, labels)), model, 1e-3, 0.8, 0.02)
      loaded <- Io.loadModel(Some(model))
      r <- Commands.replay(events, Some(labels), Some(model), None, Some(decisions), 60_000L)
      written <- Files[IO].readUtf8Lines(Path(decisions)).filter(_.nonEmpty).compile.count
      inputs <- Files[IO].readUtf8Lines(Path(events)).filter(_.nonEmpty).compile.count
      b <- Commands.backtest(events, labels, None)
    yield
      assertEquals(List(s, t, r, b), List.fill(4)(ExitCode.Success))
      assert(loaded.coefficients.nonEmpty)
      assertEquals(written, inputs)
  }

  tmp.test("malformed input lines are skipped by replay but fail strict loading") { dir =>
    val events = (dir / "bad.ndjson").toString
    for
      _ <- Io.writeText(events, "{\"id\": 1}\nnot json\n")
      r <- Commands.replay(events, None, None, None, None, 0L)
      strict <- Io.strict(Io.transactions(events), events).attempt
    yield
      assertEquals(r, ExitCode.Success)
      assert(strict.left.exists(_.getMessage.contains("line 1")), strict.toString)
  }

  tmp.test("check-rules reports errors with an error exit code") { dir =>
    val good = (dir / "good.rules").toString
    val bad = (dir / "bad.rules").toString
    for
      _ <- Io.writeText(good, "rule r when amount_usd > 1 then score +1\n")
      _ <- Io.writeText(bad, "rule r when amount_usd > then approve\n")
      g <- Commands.checkRules(good)
      b <- Commands.checkRules(bad)
    yield assertEquals((g, b), (ExitCode.Success, ExitCode.Error))
  }

  test("loading a missing rules file fails loudly") {
    Io.loadRules(Some("/nonexistent/rules")).attempt.map(r => assert(r.isLeft))
  }

  test("a model file that does not decode is rejected with its origin") {
    Io.parseModel("{}", "test.json").attempt.map { r =>
      assert(r.left.exists(_.getMessage.startsWith("test.json")), r.toString)
    }
  }
