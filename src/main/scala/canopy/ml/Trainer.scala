package canopy.ml

import canopy.domain.{Label, Outcome, Transaction}
import canopy.engine.{EngineState, FeaturePipeline}
import canopy.features.{FeatureSchema, FeatureVector}

final case class LabeledRow(transactionId: String, timestamp: Long, features: FeatureVector, label: Int)

final case class TrainingResult(
    model: LogisticModel,
    fit: FitResult,
    train: Metrics.Report,
    validation: Metrics.Report,
    tuned: ThresholdTuner.Tuned,
    trainRows: Int,
    validationRows: Int
)

final case class TrainerConfig(
    validationShare: Double = 0.25,
    declinePrecision: Double = 0.9,
    maxReviewRate: Double = 0.02,
    train: TrainConfig = TrainConfig()
)

/**
 * Offline training: replays history through the serving feature pipeline, splits by time, fits
 * preprocessing and a logistic model on the earlier part, and measures and tunes on the later.
 *
 * Chargeback labels feed the entity graph only once the replay clock has passed their report
 * time, so graph features at training time contain exactly what serving would have known. The
 * target itself is the eventual outcome, which is the point of supervised training.
 */
object Trainer:
  def buildRows(
      transactions: Seq[Transaction],
      labels: Seq[Label],
      pipeline: FeaturePipeline = FeaturePipeline()
  ): Vector[LabeledRow] =
    val fraud = labels.iterator.filter(_.outcome == Outcome.Fraud).map(_.transactionId).toSet
    val pendingLabels = labels.sortBy(_.reportedAt).toVector
    val ordered = transactions.sortBy(t => (t.timestamp, t.id))
    val out = Vector.newBuilder[LabeledRow]
    var state: EngineState = pipeline.emptyState
    var nextLabel = 0
    ordered.foreach { tx =>
      while nextLabel < pendingLabels.size && !pendingLabels(nextLabel).reportedAt.isAfter(tx.timestamp) do
        state = pipeline.label(state, pendingLabels(nextLabel))
        nextLabel += 1
      val (next, fv) = pipeline.observe(state, tx)
      state = next
      out += LabeledRow(tx.id, tx.timestamp.toEpochMilli, fv, if fraud.contains(tx.id) then 1 else 0)
    }
    out.result()

  /** Splits rows chronologically; the latest `share` of the timeline becomes validation. */
  def timeSplit(rows: Vector[LabeledRow], share: Double): (Vector[LabeledRow], Vector[LabeledRow]) =
    require(share > 0 && share < 1, "validation share must be in (0, 1)")
    val sorted = rows.sortBy(_.timestamp)
    val cut = ((1 - share) * sorted.size).toInt
    sorted.splitAt(cut)

  def train(
      rows: Vector[LabeledRow],
      config: TrainerConfig = TrainerConfig(),
      features: Vector[String] = FeatureSchema.modelInputs,
      version: String = "logreg-v1"
  ): TrainingResult =
    val (trainRows, validRows) = timeSplit(rows, config.validationShare)
    require(trainRows.exists(_.label == 1), "training split contains no positive examples")
    require(validRows.nonEmpty, "validation split is empty")
    val pre = Preprocessor.fit(features, trainRows.map(_.features))
    val x = trainRows.map(r => pre.row(r.features)).toArray
    val y = trainRows.map(_.label).toArray
    val fit = LogisticRegression.fit(x, y, config.train)
    val provisional = LogisticModel(version, pre, fit.intercept, fit.coefficients.toVector, Thresholds(0.5, 0.9), None)
    def scores(rs: Vector[LabeledRow]) = rs.map(r => provisional.score(r.features).probability).toArray
    val trainScores = scores(trainRows)
    val validScores = scores(validRows)
    val validLabels = validRows.map(_.label).toArray
    val validationReport = Metrics.report(validScores, validLabels)
    val tuned = ThresholdTuner.tune(validScores, validLabels, config.declinePrecision, config.maxReviewRate)
    TrainingResult(
      model = provisional.copy(thresholds = tuned.thresholds, validation = Some(validationReport)),
      fit = fit,
      train = Metrics.report(trainScores, y),
      validation = validationReport,
      tuned = tuned,
      trainRows = trainRows.size,
      validationRows = validRows.size
    )
