package canopy.cli

import canopy.api.Server
import canopy.codec.JsonCodecs.given
import canopy.domain.{Outcome, Verdict}
import canopy.engine.{FeaturePipeline, ScoringService}
import canopy.ingest.{Ordered, StreamOps}
import canopy.ml.{Trainer, TrainerConfig, TrainConfig}
import canopy.rules.{Backtest, RuleSet, Severity}
import canopy.sim.{SimConfig, SimResult, Simulator}
import cats.effect.{ExitCode, IO}
import cats.syntax.all.*
import io.circe.syntax.*

import java.util.concurrent.TimeUnit

/** Implementations of the CLI subcommands. Each returns an exit code. */
object Commands:
  def serve(host: String, port: Int, model: Option[String], rules: Option[String]): IO[ExitCode] =
    for
      m <- Io.loadModel(model)
      r <- Io.loadRules(rules)
      service <- ScoringService.create(m, r)
      _ <- Server.resource(service, host, port).use { server =>
        IO.println(
          s"canopy-risk listening on ${server.address} (model ${m.version}, ${r.size} rules)"
        ) *> IO.never
      }
    yield ExitCode.Success

  def simulate(config: SimConfig, eventsOut: String, labelsOut: String): IO[ExitCode] =
    for
      result <- IO(Simulator.run(config))
      _ <- Io.writeNdjson(eventsOut, result.transactions)
      _ <- Io.writeNdjson(labelsOut, result.labels)
      _ <- IO.println(
        s"wrote ${result.transactions.size} transactions to $eventsOut and ${result.labels.size} labels to $labelsOut"
      )
    yield ExitCode.Success

  def train(
      source: Either[SimConfig, (String, String)],
      out: String,
      l2: Double,
      declinePrecision: Double,
      maxReviewRate: Double
  ): IO[ExitCode] =
    for
      data <- source match
        case Left(cfg) => IO(Simulator.run(cfg))
        case Right((events, labels)) =>
          (
            Io.strict(Io.transactions(events), events),
            Io.strict(Io.labels(labels), labels)
          ).mapN(SimResult.apply)
      start <- IO.monotonic
      rows <- IO(Trainer.buildRows(data.transactions, data.labels))
      result <- IO(
        Trainer.train(
          rows,
          TrainerConfig(
            declinePrecision = declinePrecision,
            maxReviewRate = maxReviewRate,
            train = TrainConfig(l2 = l2)
          )
        )
      )
      end <- IO.monotonic
      _ <- Io.saveModel(out, result.model)
      _ <- IO.println(
        s"""trained on ${result.trainRows} rows, validated on ${result.validationRows} (${(end - start).toMillis} ms)
           |newton iterations: ${result.fit.iterations} (converged: ${result.fit.converged})
           |train:      ${result.train.render}
           |validation: ${result.validation.render}
           |thresholds: review >= ${"%.4f".format(
            result.model.thresholds.review
          )}, decline >= ${"%.4f".format(result.model.thresholds.decline)}
           |decline precision ${"%.3f".format(
            result.tuned.declineConfusion.precision
          )}, recall ${"%.3f".format(
            result.tuned.declineConfusion.recall
          )}, review rate ${"%.4f".format(result.tuned.reviewRate)}
           |top weights:
           |${result.model.weights.take(8).map((k, w) => f"  $k%-32s $w%+.3f").mkString("\n")}
           |model written to $out""".stripMargin
      )
    yield ExitCode.Success

  final case class ReplaySummary(
      total: Long,
      rejected: Long,
      late: Long,
      verdicts: Map[Verdict, Long],
      caughtFraud: Long,
      reviewedFraud: Long,
      knownFraud: Long,
      declinedLegit: Long
  )

  def replay(
      events: String,
      labels: Option[String],
      model: Option[String],
      rules: Option[String],
      decisionsOut: Option[String],
      maxLatenessMillis: Long
  ): IO[ExitCode] =
    for
      m <- Io.loadModel(model)
      r <- Io.loadRules(rules)
      fraud <- labels.fold(IO.pure(Set.empty[String]))(p =>
        Io.strict(
          Io.labels(p),
          p
        ).map(_.filter(_.outcome == Outcome.Fraud).map(_.transactionId).toSet)
      )
      service <- ScoringService.create(m, r, FeaturePipeline())
      start <- IO.monotonic
      decisions <- decisionsOut.fold(IO.pure(Option.empty[java.io.Writer]))(p =>
        IO.blocking(Some(java.nio.file.Files.newBufferedWriter(java.nio.file.Path.of(p))))
      )
      rejected <- IO.ref(0L)
      summary <- Io
        .transactions(events)
        .evalTap {
          case Left(e)  => rejected.update(_ + 1) *> IO.println(s"skipping ${e.render}")
          case Right(_) => IO.unit
        }
        .collect { case Right(tx) => tx }
        .through(StreamOps.dedupe(24L * 3_600_000)(_.id, _.timestamp.toEpochMilli))
        .through(StreamOps.reorder(maxLatenessMillis)(_.timestamp.toEpochMilli))
        .evalMap {
          case Ordered.InOrder(tx) => service.score(tx).tupleRight(false)
          case Ordered.Late(tx, lag) =>
            IO.println(s"${tx.id} arrived $lag ms too late to reorder; scoring anyway") *>
              service.score(tx).tupleRight(true)
        }
        .evalTap((d, _) =>
          decisions.fold(IO.unit)(w => IO.blocking(w.write(d.asJson.noSpaces + "\n")))
        )
        .compile
        .fold(ReplaySummary(0, 0, 0, Map.empty, 0, 0, fraud.size.toLong, 0)) {
          case (s, (d, late)) =>
            val isFraud = fraud.contains(d.transactionId)
            s.copy(
              total = s.total + 1,
              late = s.late + (if late then 1 else 0),
              verdicts = s.verdicts.updatedWith(d.verdict)(c => Some(c.getOrElse(0L) + 1)),
              caughtFraud =
                s.caughtFraud + (if isFraud && d.verdict == Verdict.Decline then 1 else 0),
              reviewedFraud =
                s.reviewedFraud + (if isFraud && d.verdict == Verdict.Review then 1 else 0),
              declinedLegit =
                s.declinedLegit + (if !isFraud && d.verdict == Verdict.Decline then 1 else 0)
            )
        }
        .guarantee(decisions.fold(IO.unit)(w => IO.blocking(w.close())))
      end <- IO.monotonic
      elapsed = (end - start).toUnit(TimeUnit.SECONDS)
      rejectedCount <- rejected.get
      _ <-
        IO.println(renderSummary(summary.copy(rejected = rejectedCount), elapsed, labels.isDefined))
    yield ExitCode.Success

  private def renderSummary(s: ReplaySummary, seconds: Double, withLabels: Boolean): String =
    val counts = Verdict.values.map(v => s"${v.wire}=${s.verdicts.getOrElse(v, 0L)}").mkString(" ")
    val rate = if seconds > 0 then s.total / seconds else 0.0
    val base = f"scored ${s.total} transactions in $seconds%.2f s ($rate%.0f tx/s): $counts" +
      s"\nrejected ${s.rejected} malformed lines, ${s.late} events arrived too late to reorder"
    if !withLabels then base
    else
      val known = math.max(1L, s.knownFraud)
      val declined = math.max(1L, s.verdicts.getOrElse(Verdict.Decline, 0L))
      base + "\n" +
        f"fraud declined ${s.caughtFraud}/${s.knownFraud} (${100.0 * s.caughtFraud / known}%.1f%%), " +
        f"sent to review ${s.reviewedFraud}, decline precision ${100.0 * (declined - s.declinedLegit) / declined}%.1f%%"

  def backtest(events: String, labels: String, rules: Option[String]): IO[ExitCode] =
    for
      rs <- Io.loadRules(rules)
      txs <- Io.strict(Io.transactions(events), events)
      ls <- Io.strict(Io.labels(labels), labels)
      rows <- IO(Trainer.buildRows(txs, ls))
      report = Backtest.run(rs, rows.map(r => (r.features, r.label)))
      _ <- IO.println(report.render)
    yield ExitCode.Success

  def checkRules(path: String): IO[ExitCode] =
    Io.readText(path).flatMap { source =>
      RuleSet.compile(source) match
        case Left(ds) => IO.println(RuleSet.renderErrors(source, ds)).as(ExitCode.Error)
        case Right(rs) =>
          val warnings = rs.warnings.filter(_.severity == Severity.Warning)
          val lines = rs.rules.map(r =>
            f"  ${r.priority}%4d  ${r.name}%-28s ${r.action.render}%-12s ${r.features.toList.sorted.mkString(", ")}"
          )
          IO.println(s"$path: ${rs.size} rules OK\n${lines.mkString("\n")}") *>
            warnings.traverse_(w => IO.println(w.render(source))).as(ExitCode.Success)
    }
