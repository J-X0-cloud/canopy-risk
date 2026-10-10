package canopy.ml

import canopy.sim.{Rand, SplitMix64}

class LogisticRegressionSuite extends munit.FunSuite:
  /** Draws data from a known logistic model. */
  private def synthetic(
      n: Int,
      beta0: Double,
      beta: Array[Double],
      seed: Long
  ): (Array[Array[Double]], Array[Int]) =
    val gen =
      for
        xs <- Rand.replicate(beta.length, Rand.gaussian)
        u <- Rand.double
      yield
        val z = beta0 + xs.zip(beta).map(_ * _).sum
        (xs.toArray, if u < LogisticRegression.sigmoid(z) then 1 else 0)
    val rows = Rand.replicate(n, gen).run(SplitMix64(seed))._2
    (rows.map(_._1).toArray, rows.map(_._2).toArray)

  test("sigmoid and softplus are stable at extremes") {
    assertEqualsDouble(LogisticRegression.sigmoid(800), 1.0, 0)
    assertEqualsDouble(LogisticRegression.sigmoid(-800), 0.0, 1e-300)
    assertEqualsDouble(LogisticRegression.softplus(800), 800.0, 1e-9)
    assertEqualsDouble(LogisticRegression.softplus(-800), 0.0, 1e-300)
    assertEqualsDouble(LogisticRegression.logit(0.5), 0.0, 1e-12)
  }

  test("recovers the coefficients of a known model") {
    val truth = Array(1.5, -2.0, 0.0)
    val (x, y) = synthetic(20_000, -0.5, truth, seed = 1)
    val fit = LogisticRegression.fit(x, y, TrainConfig(l2 = 0.0, positiveWeight = Some(1.0)))
    assert(fit.converged)
    assertEqualsDouble(fit.intercept, -0.5, 0.08)
    fit.coefficients.zip(truth).foreach((got, want) => assertEqualsDouble(got, want, 0.1))
  }

  test("Newton converges in few iterations") {
    val (x, y) = synthetic(5_000, 0.3, Array(0.8, -0.4), seed = 2)
    val fit = LogisticRegression.fit(x, y, TrainConfig(positiveWeight = Some(1.0)))
    assert(fit.converged)
    assert(fit.iterations <= 10, fit.iterations)
  }

  test("prior correction keeps class-weighted probabilities calibrated") {
    val (x, y) = synthetic(30_000, -4.0, Array(1.0), seed = 3)
    val base = y.sum.toDouble / y.length
    val weighted = LogisticRegression.fit(x, y)
    val meanPred = x.map(r =>
      LogisticRegression.sigmoid(weighted.intercept + weighted.coefficients(0) * r(0))
    ).sum / x.length
    assertEqualsDouble(meanPred, base, base * 0.25)
  }

  test("perfectly separable data stays finite thanks to L2") {
    val x = Array.tabulate(200)(i => Array(if i < 100 then -1.0 - i * 0.01 else 1.0 + i * 0.01))
    val y = Array.tabulate(200)(i => if i < 100 then 0 else 1)
    val fit = LogisticRegression.fit(x, y, TrainConfig(l2 = 1e-2))
    assert(fit.coefficients.forall(c => !c.isNaN && !c.isInfinite))
    assert(fit.coefficients(0) > 1.0)
  }

  test("duplicated columns do not break the solver") {
    val (x0, y) = synthetic(2_000, 0.0, Array(1.0), seed = 4)
    val x = x0.map(r => Array(r(0), r(0)))
    val fit = LogisticRegression.fit(x, y, TrainConfig(l2 = 1e-4, positiveWeight = Some(1.0)))
    assertEqualsDouble(fit.coefficients.sum, 1.0, 0.15)
  }

  test("input validation") {
    intercept[IllegalArgumentException](LogisticRegression.fit(Array.empty, Array.empty))
    intercept[IllegalArgumentException](LogisticRegression.fit(Array(Array(1.0)), Array(2)))
  }
