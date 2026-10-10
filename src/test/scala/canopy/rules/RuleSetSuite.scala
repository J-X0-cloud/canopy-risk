package canopy.rules

import canopy.features.{FeatureValue, FeatureVector}

class RuleSetSuite extends munit.FunSuite:
  private val source =
    """let tiny = amount_usd < 5
      |
      |rule low_priority_score
      |  when merchant.high_risk
      |  then score +0.5
      |
      |rule card_testing priority 90
      |  when tiny and card.count_10m >= 5
      |  then decline
      |  reason "Burst of small authorizations"
      |  tag "card-testing"
      |
      |rule trusted priority 10
      |  when card.age_days > 365 and not geo_mismatch
      |  then approve
      |""".stripMargin

  private val rules = RuleSet.compile(source).fold(ds => fail(RuleSet.renderErrors(source, ds)), identity)

  test("rules are ordered by priority, then source order") {
    assertEquals(rules.rules.map(_.name).toList, List("card_testing", "trusted", "low_priority_score"))
  }

  test("only definitely-true conditions fire") {
    val fv = FeatureVector(
      Map(
        "amount_usd" -> FeatureValue.Num(1.0),
        "card.count_10m" -> FeatureValue.Num(7),
        "merchant.high_risk" -> FeatureValue.Bool(true),
        "card.age_days" -> FeatureValue.Num(400)
      )
    )
    val fired = rules.evaluate(fv)
    assertEquals(fired.map(_.rule), List("card_testing", "low_priority_score"))
    assertEquals(fired.head.tags, List("card-testing"))
    assertEquals(fired.head.reason, "Burst of small authorizations")
    val traced = rules.trace(fv).toMap
    assertEquals(traced("trusted"), Value.Missing)
  }

  test("rules without a reason fall back to their name") {
    val fv = FeatureVector(Map("card.age_days" -> FeatureValue.Num(400), "geo_mismatch" -> FeatureValue.Bool(false)))
    assertEquals(rules.evaluate(fv).map(_.reason), List("trusted"))
  }

  test("compile errors are rendered with source context") {
    val bad = "rule r\n  when card.cnt_1h > 3\n  then approve"
    val errs = RuleSet.compile(bad).swap.toOption.get
    val rendered = RuleSet.renderErrors(bad, errs)
    assert(rendered.contains("did you mean 'card.count_1h'"), rendered)
    assert(rendered.contains("2 |   when card.cnt_1h > 3"), rendered)
  }

  test("the bundled default rules compile cleanly") {
    val text = scala.io.Source.fromResource("default.rules").mkString
    val rs = RuleSet.compile(text).fold(ds => fail(RuleSet.renderErrors(text, ds)), identity)
    assert(rs.size >= 5)
    assertEquals(rs.warnings, Nil)
  }
