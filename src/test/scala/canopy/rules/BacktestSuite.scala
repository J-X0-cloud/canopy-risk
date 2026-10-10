package canopy.rules

import canopy.features.{FeatureValue, FeatureVector}

class BacktestSuite extends munit.FunSuite:
  private val rules = RuleSet
    .compile(
      """rule big priority 10 when amount_usd > 100 then review reason "big"
        |rule night when is_night then score +1
        |""".stripMargin
    )
    .toOption
    .get

  private def row(amount: Option[Double], night: Boolean, label: Int): (FeatureVector, Int) =
    val base = FeatureVector(Map("is_night" -> FeatureValue.Bool(night)))
    (amount.fold(base)(a => base.withNum("amount_usd", a)), label)

  test("hits, precision, recall and incremental catches") {
    val report = Backtest.run(
      rules,
      List(
        row(Some(500), night = true, 1), // both fire; big gets the incremental catch
        row(Some(500), night = false, 0), // big false positive
        row(Some(5), night = true, 1), // only night catches it
        row(None, night = false, 1), // big unknown, nothing fires
        row(Some(5), night = false, 0)
      )
    )
    val big = report.rules.find(_.rule == "big").get
    val night = report.rules.find(_.rule == "night").get
    assertEquals((big.hits, big.fraudHits, big.incrementalFraud, big.unknown), (2, 1, 1, 1))
    assertEquals((night.hits, night.fraudHits, night.incrementalFraud), (2, 2, 1))
    assertEqualsDouble(big.precision, 0.5, 1e-12)
    assertEqualsDouble(report.recall(night), 2.0 / 3, 1e-12)
    assertEquals(report.caughtByAny, 2)
    assert(report.render.contains("caught by any rule=2"))
  }

  test("an empty history produces zeroed statistics") {
    val report = Backtest.run(rules, Nil)
    assertEquals(report.rows, 0)
    assert(report.rules.forall(s => s.hits == 0 && s.precision == 0.0))
  }
