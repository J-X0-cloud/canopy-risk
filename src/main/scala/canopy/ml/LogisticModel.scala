package canopy.ml

import canopy.domain.Contribution
import canopy.features.FeatureVector

/** Output of scoring one feature vector. */
final case class Score(probability: Double, logit: Double, contributions: Vector[Contribution])

/**
 * A trained, serialisable scoring model: preprocessing plus coefficients over its columns.
 *
 * Contributions are reported per original feature (a feature's value column and its missing
 * indicator are summed), as coefficient times standardised value. Because inputs are centred on
 * the training mean, a contribution reads as "how much this feature moved the log-odds relative to
 * an average transaction".
 */
final case class LogisticModel(
    version: String,
    preprocessor: Preprocessor,
    intercept: Double,
    coefficients: Vector[Double],
    thresholds: Thresholds,
    validation: Option[Metrics.Report]
):
  require(
    coefficients.size == preprocessor.width,
    s"model has ${coefficients.size} coefficients for ${preprocessor.width} columns"
  )

  def score(fv: FeatureVector): Score =
    val row = preprocessor.row(fv)
    var z = intercept
    var i = 0
    while i < row.length do
      z += coefficients(i) * row(i)
      i += 1
    var col = 0
    val contributions = preprocessor.specs.map { spec =>
      val raw = fv.num(spec.name)
      var c = coefficients(col) * row(col)
      if spec.missingIndicator then c += coefficients(col + 1) * row(col + 1)
      col += (if spec.missingIndicator then 2 else 1)
      Contribution(spec.name, raw, c)
    }
    Score(LogisticRegression.sigmoid(z), z, contributions)

  /** Coefficients by column name, largest magnitude first. */
  def weights: Vector[(String, Double)] =
    preprocessor.columnNames.zip(coefficients).sortBy(-_._2.abs)
