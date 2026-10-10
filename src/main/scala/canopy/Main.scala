package canopy

import canopy.cli.Commands
import canopy.sim.SimConfig
import cats.effect.{ExitCode, IO}
import cats.syntax.all.*
import com.monovore.decline.*
import com.monovore.decline.effect.CommandIOApp

object Main
    extends CommandIOApp(
      name = "canopy-risk",
      header = "Streaming payment-fraud scoring engine",
      version = "0.1.0"
    ):

  private val modelOpt =
    Opts.option[String]("model", "Model JSON file (defaults to the bundled model).").orNone
  private val rulesOpt =
    Opts.option[String]("rules", "Rules file (defaults to the bundled rules).").orNone

  private val simOpts: Opts[SimConfig] =
    (
      Opts.option[Long]("seed", "Random seed.").withDefault(7L),
      Opts.option[Int]("cardholders", "Number of simulated cardholders.").withDefault(2000),
      Opts.option[Int]("days", "Days of activity to simulate.").withDefault(30)
    ).mapN((seed, holders, days) =>
      val scale = holders / 2000.0 * days / 30.0
      SimConfig(
        seed = seed,
        cardholders = holders,
        days = days,
        cardTestingAttacks = math.max(1, (40 * scale).round.toInt),
        takeovers = math.max(1, (40 * scale).round.toInt),
        fraudRings = math.max(1, (6 * scale).round.toInt)
      )
    )

  private val serve = Opts.subcommand("serve", "Run the HTTP API.") {
    val port = Opts.option[Int]("port", "Port to listen on (default: $PORT or 8080).").orNone
    val host = Opts.option[String]("host", "Interface to bind.").withDefault("0.0.0.0")
    (host, port, modelOpt, rulesOpt).mapN { (h, p, m, r) =>
      val resolved = p.orElse(sys.env.get("PORT").flatMap(_.toIntOption)).getOrElse(8080)
      Commands.serve(h, resolved, m, r)
    }
  }

  private val simulate = Opts.subcommand("simulate", "Generate labelled synthetic transactions.") {
    (
      simOpts,
      Opts.option[String]("events", "Output NDJSON file for transactions."),
      Opts.option[String]("labels", "Output NDJSON file for labels.")
    ).mapN(Commands.simulate)
  }

  private val train = Opts.subcommand("train", "Train a logistic model and write it as JSON.") {
    val fromFiles = (
      Opts.option[String]("events", "Transactions NDJSON."),
      Opts.option[String]("labels", "Labels NDJSON.")
    ).tupled.map(Right(_))
    val fromSim = simOpts.map(Left(_))
    (
      fromFiles.orElse(fromSim),
      Opts.option[String]("out", "Where to write the model."),
      Opts.option[Double]("l2", "L2 penalty.").withDefault(1e-3),
      Opts.option[Double](
        "decline-precision",
        "Required precision at the decline threshold."
      ).withDefault(0.8),
      Opts.option[Double](
        "max-review-rate",
        "Share of traffic the review queue can absorb."
      ).withDefault(0.02)
    ).mapN(Commands.train)
  }

  private val replay = Opts.subcommand("replay", "Score an NDJSON file through the full engine.") {
    (
      Opts.option[String]("events", "Transactions NDJSON."),
      Opts.option[String]("labels", "Labels NDJSON, for catch-rate reporting.").orNone,
      modelOpt,
      rulesOpt,
      Opts.option[String]("decisions", "Write decisions as NDJSON to this file.").orNone,
      Opts.option[Long](
        "max-lateness-ms",
        "Reorder window for out-of-order events."
      ).withDefault(60_000L)
    ).mapN(Commands.replay)
  }

  private val checkRules = Opts.subcommand("check-rules", "Type check a rules file.") {
    Opts.argument[String]("file").map(Commands.checkRules)
  }

  def main: Opts[IO[ExitCode]] = serve orElse simulate orElse train orElse replay orElse checkRules
