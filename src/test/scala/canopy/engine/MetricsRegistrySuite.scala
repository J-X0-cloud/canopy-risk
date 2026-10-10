package canopy.engine

import canopy.domain.*
import munit.CatsEffectSuite

class MetricsRegistrySuite extends CatsEffectSuite:
  private def decision(v: Verdict, hits: List[String]) =
    Decision(
      "t",
      v,
      0.1,
      0.1,
      "model",
      Explanation("", Nil, Nil, hits.map(h => RuleHit(h, "review", "r"))),
      Nil
    )

  test("histogram buckets and quantiles") {
    val h = List(0.0002, 0.0002, 0.003, 0.2, 5.0).foldLeft(Histogram.empty())(_.observe(_))
    assertEquals(h.count, 5L)
    assertEquals(h.quantile(0.4), Some(0.00025))
    assertEquals(h.quantile(0.6), Some(0.005))
    assertEquals(h.quantile(1.0), Some(Double.PositiveInfinity))
    assertEquals(Histogram.empty().quantile(0.5), None)
  }

  test("prometheus exposition has cumulative buckets and escaped labels") {
    for
      m <- MetricsRegistry.create
      _ <- m.recordDecision(decision(Verdict.Review, List("we\"ird")), 0.0003)
      _ <- m.recordDecision(decision(Verdict.Approve, Nil), 0.02)
      _ <- m.recordLabel
      text <- m.render(List(("canopy_test_gauge", "A gauge.", 3.0)))
    yield
      assert(text.contains("""canopy_decisions_total{verdict="review"} 1"""), text)
      assert(text.contains("""canopy_rule_hits_total{rule="we\"ird"} 1"""), text)
      assert(text.contains("""canopy_scoring_seconds_bucket{le="0.0005"} 1"""), text)
      assert(text.contains("""canopy_scoring_seconds_bucket{le="+Inf"} 2"""), text)
      assert(text.contains("canopy_labels_total 1"), text)
      assert(text.contains("canopy_test_gauge 3"), text)
  }

  test("idempotency cache evicts oldest entries first") {
    val c = (1 to 4).foldLeft(IdempotencyCache.empty[Int](3))((acc, i) => acc.put(s"k$i", i))
    assertEquals(c.get("k1"), None)
    assertEquals(c.get("k4"), Some(4))
    assertEquals(c.put("k2", 20).get("k2"), Some(20))
    assertEquals(c.size, 3)
  }
