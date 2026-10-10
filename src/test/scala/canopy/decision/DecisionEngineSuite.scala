package canopy.decision

import canopy.domain.{Contribution, Verdict}
import canopy.ml.{LogisticRegression, Score, Thresholds}
import canopy.rules.{Action, Firing}

class DecisionEngineSuite extends munit.FunSuite:
  private val policy = Policy(Thresholds(review = 0.2, decline = 0.8))

  private def score(p: Double, contributions: Vector[Contribution] = Vector.empty): Score =
    Score(p, LogisticRegression.logit(p), contributions)

  private def firing(name: String, action: Action, priority: Int = 0, tags: List[String] = Nil) =
    Firing(name, priority, action, s"reason for $name", tags)

  test("model thresholds decide when no hard rule fires") {
    assertEquals(DecisionEngine.decide("t", score(0.05), Nil, policy).verdict, Verdict.Approve)
    assertEquals(DecisionEngine.decide("t", score(0.2), Nil, policy).verdict, Verdict.Review)
    assertEquals(DecisionEngine.decide("t", score(0.8), Nil, policy).verdict, Verdict.Decline)
    assertEquals(DecisionEngine.decide("t", score(0.5), Nil, policy).decidedBy, "model")
  }

  test("score adjustments compose in log-odds space") {
    val d = DecisionEngine.decide(
      "t",
      score(0.1),
      List(firing("a", Action.Score(1.0)), firing("b", Action.Score(1.0))),
      policy
    )
    val expected = LogisticRegression.sigmoid(LogisticRegression.logit(0.1) + 2.0)
    assertEqualsDouble(d.finalScore, expected, 1e-12)
    assertEqualsDouble(d.modelScore, 0.1, 1e-12)
    assertEquals(d.verdict, Verdict.Review)
  }

  test("hard rules override the model and say so") {
    val d = DecisionEngine.decide("t", score(0.01), List(firing("ct", Action.Decline, 100)), policy)
    assertEquals(d.verdict, Verdict.Decline)
    assertEquals(d.decidedBy, "rule:ct")
    assert(d.explanation.summary.contains("model alone would approve"), d.explanation.summary)
  }

  test("highest priority group wins, most severe within the group") {
    val firings = List(
      firing("allow", Action.Approve, 50),
      firing("block", Action.Decline, 50),
      firing("low", Action.Decline, 1)
    )
    val d = DecisionEngine.decide("t", score(0.5), firings, policy)
    assertEquals(d.decidedBy, "rule:block")
    val d2 = DecisionEngine.decide(
      "t",
      score(0.5),
      List(firing("allow", Action.Approve, 90), firing("low", Action.Decline, 1)),
      policy
    )
    assertEquals(d2.verdict, Verdict.Approve)
  }

  test("approve rules cannot wave through scores above the ceiling") {
    val d =
      DecisionEngine.decide("t", score(0.995), List(firing("vip", Action.Approve, 10)), policy)
    assertEquals(d.verdict, Verdict.Decline)
    assertEquals(d.decidedBy, "model")
    assert(d.explanation.summary.contains("vip ignored"), d.explanation.summary)
  }

  test("explanations rank factors and drop negligible ones") {
    val contributions = Vector(
      Contribution("a", Some(1), 0.9),
      Contribution("b", Some(2), 0.01),
      Contribution("c", None, -0.4),
      Contribution("d", Some(3), 1.5),
      Contribution("e", Some(0), -1.2)
    )
    val d = DecisionEngine.decide("t", score(0.9, contributions), Nil, policy)
    assertEquals(d.explanation.topRiskFactors.map(_.feature), List("d", "a"))
    assertEquals(d.explanation.topMitigatingFactors.map(_.feature), List("e", "c"))
    assert(d.explanation.summary.contains("main risk factors: d, a"), d.explanation.summary)
  }

  test("tags are collected without duplicates and every firing is reported") {
    val firings = List(
      firing("a", Action.Score(0.5), tags = List("x", "y")),
      firing("b", Action.Review, 5, List("y"))
    )
    val d = DecisionEngine.decide("t", score(0.1), firings, policy)
    assertEquals(d.tags, List("x", "y"))
    assertEquals(d.explanation.ruleHits.map(_.action), List("score +0.5", "review"))
  }
