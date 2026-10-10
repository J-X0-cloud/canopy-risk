package canopy.ml

/** Evaluation metrics for probabilistic binary classifiers. */
object Metrics:
  final case class Confusion(tp: Int, fp: Int, tn: Int, fn: Int):
    def precision: Double = if tp + fp == 0 then 0.0 else tp.toDouble / (tp + fp)
    def recall: Double = if tp + fn == 0 then 0.0 else tp.toDouble / (tp + fn)
    def falsePositiveRate: Double = if fp + tn == 0 then 0.0 else fp.toDouble / (fp + tn)
    def f1: Double =
      val (p, r) = (precision, recall)
      if p + r == 0 then 0.0 else 2 * p * r / (p + r)

  final case class Report(
      count: Int,
      positives: Int,
      auc: Double,
      averagePrecision: Double,
      logLoss: Double,
      brier: Double,
      expectedCalibrationError: Double
  ):
    def render: String =
      f"n=$count positives=$positives auc=$auc%.4f ap=$averagePrecision%.4f " +
        f"logloss=$logLoss%.4f brier=$brier%.5f ece=$expectedCalibrationError%.4f"

  def report(scores: Array[Double], labels: Array[Int]): Report =
    Report(
      scores.length,
      labels.count(_ == 1),
      auc(scores, labels),
      averagePrecision(scores, labels),
      logLoss(scores, labels),
      brier(scores, labels),
      expectedCalibrationError(scores, labels)
    )

  private def check(scores: Array[Double], labels: Array[Int]): Unit =
    require(scores.length == labels.length, "scores and labels differ in length")

  /**
   * Area under the ROC curve via the Mann-Whitney U statistic. Tied scores receive their average
   * rank, which is equivalent to counting tied positive/negative pairs as one half.
   */
  def auc(scores: Array[Double], labels: Array[Int]): Double =
    check(scores, labels)
    val pos = labels.count(_ == 1)
    val neg = labels.length - pos
    if pos == 0 || neg == 0 then Double.NaN
    else
      val order = scores.indices.sortBy(scores(_)).toArray
      var rankSumPos = 0.0
      var i = 0
      while i < order.length do
        var j = i
        while j + 1 < order.length && scores(order(j + 1)) == scores(order(i)) do j += 1
        val avgRank = (i + j) / 2.0 + 1.0
        for k <- i to j do if labels(order(k)) == 1 then rankSumPos += avgRank
        i = j + 1
      (rankSumPos - pos.toDouble * (pos + 1) / 2) / (pos.toDouble * neg)

  /** Average precision: the precision-recall curve integrated as a step function. */
  def averagePrecision(scores: Array[Double], labels: Array[Int]): Double =
    check(scores, labels)
    val pos = labels.count(_ == 1)
    if pos == 0 then Double.NaN
    else
      val order = scores.indices.sortBy(i => -scores(i)).toArray
      var tp = 0
      var fp = 0
      var ap = 0.0
      var i = 0
      while i < order.length do
        // Treat a run of tied scores as a single threshold.
        var j = i
        var runTp = 0
        while j < order.length && scores(order(j)) == scores(order(i)) do
          if labels(order(j)) == 1 then runTp += 1 else fp += 1
          j += 1
        tp += runTp
        if runTp > 0 then ap += (runTp.toDouble / pos) * (tp.toDouble / (tp + fp))
        i = j
      ap

  def logLoss(scores: Array[Double], labels: Array[Int], eps: Double = 1e-15): Double =
    check(scores, labels)
    if scores.isEmpty then Double.NaN
    else
      scores.indices.map { i =>
        val p = math.min(1 - eps, math.max(eps, scores(i)))
        if labels(i) == 1 then -math.log(p) else -math.log(1 - p)
      }.sum / scores.length

  def brier(scores: Array[Double], labels: Array[Int]): Double =
    check(scores, labels)
    if scores.isEmpty then Double.NaN
    else scores.indices.map(i => math.pow(scores(i) - labels(i), 2)).sum / scores.length

  /** Expected calibration error over equal-width probability bins, weighted by bin size. */
  def expectedCalibrationError(scores: Array[Double], labels: Array[Int], bins: Int = 10): Double =
    check(scores, labels)
    if scores.isEmpty then Double.NaN
    else
      val grouped = scores.indices.groupBy(i => math.min(bins - 1, (scores(i) * bins).toInt))
      grouped.values.map { idx =>
        val conf = idx.map(scores(_)).sum / idx.size
        val acc = idx.map(labels(_)).sum.toDouble / idx.size
        idx.size * math.abs(conf - acc)
      }.sum / scores.length

  def confusion(scores: Array[Double], labels: Array[Int], threshold: Double): Confusion =
    check(scores, labels)
    scores.indices.foldLeft(Confusion(0, 0, 0, 0)) { (c, i) =>
      (scores(i) >= threshold, labels(i) == 1) match
        case (true, true)   => c.copy(tp = c.tp + 1)
        case (true, false)  => c.copy(fp = c.fp + 1)
        case (false, false) => c.copy(tn = c.tn + 1)
        case (false, true)  => c.copy(fn = c.fn + 1)
    }
