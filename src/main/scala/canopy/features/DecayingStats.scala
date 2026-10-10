package canopy.features

/**
 * Exponentially time-decayed mean and variance.
 *
 * Every observation carries weight 1 at arrival and its weight halves every `halfLifeMillis`.
 * Updates use West's weighted incremental algorithm, which is numerically stable and needs no
 * history. This lets a card's notion of "normal amount" drift with its owner's habits.
 */
final case class DecayingStats(
    halfLifeMillis: Long,
    weight: Double,
    mean: Double,
    m2: Double,
    observations: Long,
    lastTs: Long
):
  private def decayTo(ts: Long): DecayingStats =
    if observations == 0 || ts <= lastTs then this
    else
      val factor = math.pow(0.5, (ts - lastTs).toDouble / halfLifeMillis.toDouble)
      copy(weight = weight * factor, m2 = m2 * factor, lastTs = ts)

  def update(ts: Long, x: Double): DecayingStats =
    val d = decayTo(ts)
    val newWeight = d.weight + 1.0
    val delta = x - d.mean
    val newMean = d.mean + delta / newWeight
    val newM2 = d.m2 + delta * (x - newMean)
    DecayingStats(halfLifeMillis, newWeight, newMean, newM2, observations + 1, math.max(ts, lastTs))

  def variance: Double = if weight <= 0 then 0.0 else math.max(0.0, m2 / weight)

  def stdDev: Double = math.sqrt(variance)

  /**
   * Standard score of `x` against the history, or None while fewer than `minObservations` have been
   * seen. A variance floor stops a card that always spends the same amount from producing enormous
   * scores on a trivially different amount.
   */
  def zScore(x: Double, minObservations: Int = 3, varianceFloor: Double = 0.05): Option[Double] =
    if observations < minObservations then None
    else Some((x - mean) / math.sqrt(math.max(variance, varianceFloor)))

object DecayingStats:
  def empty(halfLifeMillis: Long): DecayingStats =
    require(halfLifeMillis > 0, "half-life must be positive")
    DecayingStats(halfLifeMillis, 0.0, 0.0, 0.0, 0L, Long.MinValue)
