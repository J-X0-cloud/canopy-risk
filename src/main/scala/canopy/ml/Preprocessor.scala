package canopy.ml

import canopy.features.FeatureVector

/** How a raw feature is reshaped before standardisation. */
enum Transform:
  case Identity

  /** ln(1 + x) for non-negative, heavily right-skewed features such as counts and amounts. */
  case Log1p

  def apply(x: Double): Double = this match
    case Identity => x
    case Log1p    => math.log1p(math.max(0.0, x))

/**
 * Per-feature preprocessing learned from training data.
 *
 * @param fill
 *   value substituted when the feature is missing (the training mean, so it standardises to 0)
 * @param missingIndicator
 *   whether an extra 0/1 "was missing" column follows this feature in the design matrix
 */
final case class FeatureSpec(
    name: String,
    transform: Transform,
    mean: Double,
    std: Double,
    missingIndicator: Boolean
)

/**
 * Maps feature vectors to dense, standardised design rows. Standardised values are clipped to
 * [-clip, clip] so a single extreme feature cannot dominate a score at serving time.
 */
final case class Preprocessor(specs: Vector[FeatureSpec], clip: Double):
  val width: Int = specs.map(s => if s.missingIndicator then 2 else 1).sum

  val columnNames: Vector[String] = specs.flatMap { s =>
    if s.missingIndicator then Vector(s.name, s"${s.name}:missing") else Vector(s.name)
  }

  def row(fv: FeatureVector): Array[Double] =
    val out = Array.ofDim[Double](width)
    var j = 0
    specs.foreach { s =>
      fv.num(s.name).filterNot(v => v.isNaN || v.isInfinite) match
        case Some(raw) =>
          val z = (s.transform(raw) - s.mean) / s.std
          out(j) = math.max(-clip, math.min(clip, z))
          if s.missingIndicator then out(j + 1) = 0.0
        case None =>
          out(j) = 0.0
          if s.missingIndicator then out(j + 1) = 1.0
      j += (if s.missingIndicator then 2 else 1)
    }
    out

object Preprocessor:
  /**
   * Learns transforms, means and standard deviations from training rows. A feature is log
   * transformed when it is never negative and its sample skewness exceeds `skewThreshold`. Constant
   * features keep std = 1 so they standardise to 0 instead of dividing by zero.
   */
  def fit(
      features: Vector[String],
      rows: Iterable[FeatureVector],
      clip: Double = 6.0,
      skewThreshold: Double = 2.0
  ): Preprocessor =
    val specs = features.map { name =>
      val values =
        rows.iterator.flatMap(_.num(name)).filterNot(v => v.isNaN || v.isInfinite).toArray
      val anyMissing = values.length < rows.size
      if values.isEmpty then FeatureSpec(name, Transform.Identity, 0.0, 1.0, anyMissing)
      else
        val transform =
          if values.forall(_ >= 0) && skewness(values) > skewThreshold then Transform.Log1p
          else Transform.Identity
        val t = values.map(transform(_))
        val mean = t.sum / t.length
        val variance = t.map(x => (x - mean) * (x - mean)).sum / t.length
        val std = if variance > 1e-12 then math.sqrt(variance) else 1.0
        FeatureSpec(name, transform, mean, std, anyMissing)
    }
    Preprocessor(specs, clip)

  def skewness(xs: Array[Double]): Double =
    val n = xs.length.toDouble
    if n < 3 then 0.0
    else
      val mean = xs.sum / n
      val m2 = xs.map(x => math.pow(x - mean, 2)).sum / n
      val m3 = xs.map(x => math.pow(x - mean, 3)).sum / n
      if m2 <= 1e-12 then 0.0 else m3 / math.pow(m2, 1.5)
