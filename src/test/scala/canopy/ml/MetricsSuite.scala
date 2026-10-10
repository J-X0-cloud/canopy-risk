package canopy.ml

class MetricsSuite extends munit.FunSuite:
  test("AUC of a perfect, a reversed and a random-like ranking") {
    val labels = Array(0, 0, 1, 1)
    assertEqualsDouble(Metrics.auc(Array(0.1, 0.2, 0.8, 0.9), labels), 1.0, 1e-12)
    assertEqualsDouble(Metrics.auc(Array(0.9, 0.8, 0.2, 0.1), labels), 0.0, 1e-12)
    assertEqualsDouble(Metrics.auc(Array(0.5, 0.5, 0.5, 0.5), labels), 0.5, 1e-12)
  }

  test("AUC counts tied positive/negative pairs as one half") {
    // pairs: (p=0.4 vs n=0.4) tie -> 0.5, (p=0.4 vs n=0.1) -> 1, (p=0.9 vs both) -> 2 => 3.5 / 4
    val auc = Metrics.auc(Array(0.1, 0.4, 0.4, 0.9), Array(0, 0, 1, 1))
    assertEqualsDouble(auc, 3.5 / 4, 1e-12)
  }

  test("AUC matches brute-force pair counting on random data") {
    val rnd = scala.util.Random(9)
    val scores = Array.fill(300)(math.round(rnd.nextDouble() * 20) / 20.0)
    val labels = Array.fill(300)(if rnd.nextDouble() < 0.3 then 1 else 0)
    val pos = scores.indices.filter(labels(_) == 1)
    val neg = scores.indices.filter(labels(_) == 0)
    val pairs =
      for p <- pos; n <- neg
      yield if scores(p) > scores(n) then 1.0 else if scores(p) == scores(n) then 0.5 else 0.0
    assertEqualsDouble(Metrics.auc(scores, labels), pairs.sum / pairs.size, 1e-12)
  }

  test("AUC is undefined with one class") {
    assert(Metrics.auc(Array(0.1, 0.2), Array(1, 1)).isNaN)
  }

  test("average precision") {
    // ranked: 1 (P=1/1), 0, 1 (P=2/3) -> AP = (1 + 2/3) / 2
    val ap = Metrics.averagePrecision(Array(0.9, 0.8, 0.7), Array(1, 0, 1))
    assertEqualsDouble(ap, (1.0 + 2.0 / 3) / 2, 1e-12)
  }

  test("log loss, brier and calibration error") {
    val s = Array(0.8, 0.2)
    val y = Array(1, 0)
    assertEqualsDouble(Metrics.logLoss(s, y), -math.log(0.8), 1e-12)
    assertEqualsDouble(Metrics.brier(s, y), 0.04, 1e-12)
    assertEqualsDouble(
      Metrics.expectedCalibrationError(Array(0.25, 0.25, 0.25, 0.25), Array(1, 0, 0, 0)),
      0.0,
      1e-12
    )
    assert(Metrics.logLoss(Array(0.0), Array(1)).isFinite)
  }

  test("confusion matrix and derived rates") {
    val c = Metrics.confusion(Array(0.9, 0.6, 0.4, 0.1), Array(1, 0, 1, 0), 0.5)
    assertEquals(c, Metrics.Confusion(tp = 1, fp = 1, tn = 1, fn = 1))
    assertEqualsDouble(c.precision, 0.5, 1e-12)
    assertEqualsDouble(c.recall, 0.5, 1e-12)
    assertEqualsDouble(c.f1, 0.5, 1e-12)
    assertEqualsDouble(c.falsePositiveRate, 0.5, 1e-12)
  }

  test("threshold tuner respects decline precision and review budget") {
    val scores = Array(0.99, 0.95, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2) ++ Array.fill(90)(0.01)
    val labels = Array(1, 1, 1, 0, 1, 0, 0, 0, 0, 0) ++ Array.fill(90)(0)
    val tuned = ThresholdTuner.tune(scores, labels, declinePrecision = 0.8, maxReviewRate = 0.03)
    assertEqualsDouble(tuned.thresholds.decline, 0.7, 1e-12)
    assertEqualsDouble(tuned.thresholds.review, 0.4, 1e-12)
    assertEqualsDouble(tuned.reviewRate, 0.03, 1e-12)
    assert(tuned.declineConfusion.precision >= 0.8)
  }

  test("threshold tuner never exceeds the review budget across ties") {
    val scores = Array(0.9) ++ Array.fill(10)(0.5) ++ Array.fill(89)(0.1)
    val labels = Array(1) ++ Array.fill(99)(0)
    val tuned = ThresholdTuner.tune(scores, labels, declinePrecision = 0.9, maxReviewRate = 0.05)
    assert(tuned.reviewRate <= 0.05, tuned.toString)
  }

  test("tuner declines nothing when no threshold is precise enough") {
    val tuned = ThresholdTuner.tune(
      Array(0.9, 0.8),
      Array(0, 1),
      declinePrecision = 0.99,
      maxReviewRate = 0.5
    )
    assertEqualsDouble(tuned.thresholds.decline, 1.0, 0)
  }
