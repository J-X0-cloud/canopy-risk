package canopy.ml

/** Probability cut-offs used by the decision policy. */
final case class Thresholds(review: Double, decline: Double):
  require(0 <= review && review <= decline && decline <= 1, s"invalid thresholds $review/$decline")

/**
 * Chooses operating points from validation scores.
 *
 * The decline threshold is the lowest score at which precision stays at or above
 * `declinePrecision`, because auto-declining a good customer is the costliest error. The review
 * threshold is then lowered as far as the analyst budget allows: at most `maxReviewRate` of all
 * traffic may land between the two thresholds.
 */
object ThresholdTuner:
  final case class Tuned(thresholds: Thresholds, declineConfusion: Metrics.Confusion, reviewRate: Double)

  def tune(
      scores: Array[Double],
      labels: Array[Int],
      declinePrecision: Double = 0.9,
      maxReviewRate: Double = 0.02
  ): Tuned =
    require(scores.length == labels.length && scores.nonEmpty, "need scored validation data")
    val n = scores.length
    val order = scores.indices.sortBy(i => -scores(i)).toArray
    // Walk thresholds from the top. Only cut at boundaries between distinct scores.
    var tp = 0
    var fp = 0
    var bestDeclineIdx = -1
    var i = 0
    while i < n do
      var j = i
      while j < n && scores(order(j)) == scores(order(i)) do
        if labels(order(j)) == 1 then tp += 1 else fp += 1
        j += 1
      if tp.toDouble / (tp + fp) >= declinePrecision then bestDeclineIdx = j - 1
      i = j
    val decline =
      if bestDeclineIdx < 0 then 1.0
      else scores(order(bestDeclineIdx))
    val declined = scores.count(_ >= decline)
    val budget = math.floor(maxReviewRate * n).toInt
    val reviewIdx = math.min(n - 1, declined + budget - 1)
    val candidate = if budget <= 0 || reviewIdx < declined then decline else scores(order(reviewIdx))
    // If the cut lands inside a run of ties, step up so the budget is not exceeded.
    val review =
      if scores.count(s => s >= candidate && s < decline) > budget then
        order.iterator.map(scores(_)).filter(s => s > candidate && s < decline).toVector.lastOption
          .getOrElse(decline)
      else candidate
    val thresholds = Thresholds(math.min(review, decline), decline)
    Tuned(
      thresholds,
      Metrics.confusion(scores, labels, decline),
      scores.count(s => s >= thresholds.review && s < thresholds.decline).toDouble / n
    )
