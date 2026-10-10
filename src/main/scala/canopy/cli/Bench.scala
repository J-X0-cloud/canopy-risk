package canopy.cli

import canopy.engine.{FeaturePipeline, ScoringService}
import canopy.features.FeatureVector
import canopy.sim.{SimConfig, Simulator}
import cats.effect.{ExitCode, IO}
import cats.syntax.all.*

/**
 * A small, honest throughput benchmark. Each stage is timed on the same simulated stream after a
 * warm-up pass so the JIT has compiled the hot paths; numbers are single-threaded wall clock.
 */
object Bench:
  final case class Stage(name: String, items: Int, nanos: Long):
    def perSecond: Double = items / (nanos / 1e9)
    def microsPerItem: Double = nanos / 1e3 / items

  private def time[A](f: => A): (A, Long) =
    val start = System.nanoTime()
    val a = f
    (a, System.nanoTime() - start)

  def run(config: SimConfig, rounds: Int): IO[ExitCode] =
    for
      model <- Io.loadModel(None)
      rules <- Io.loadRules(None)
      sim <- IO(Simulator.run(config))
      txs = sim.transactions
      pipeline = FeaturePipeline()
      replay = () =>
        txs.foldLeft((pipeline.emptyState, Vector.newBuilder[FeatureVector])) {
          case ((s, acc), tx) =>
            val (next, fv) = pipeline.observe(s, tx)
            (next, acc += fv)
        }._2.result()
      vectors <- IO(replay())
      results <- (1 to rounds).toList.traverse { _ =>
        IO {
          val (_, featureNs) = time(replay())
          val (_, ruleNs) = time(vectors.foreach(rules.evaluate))
          val (_, modelNs) = time(vectors.foreach(model.score))
          (featureNs, ruleNs, modelNs)
        }
      }
      service <- ScoringService.create(model, rules, pipeline)
      start <- IO.monotonic
      _ <- txs.traverse_(service.score)
      end <- IO.monotonic
      best = results.drop(1).minByOption(_._1).getOrElse(results.head)
      stages = List(
        Stage("feature pipeline (windows + graph)", txs.size, best._1),
        Stage(s"rule evaluation (${rules.size} rules)", vectors.size, best._2),
        Stage("model scoring + contributions", vectors.size, best._3),
        Stage("end-to-end ScoringService.score", txs.size, (end - start).toNanos)
      )
      _ <- IO.println(
        s"${txs.size} simulated transactions, ${rounds} rounds (best of rounds after warm-up)"
      )
      _ <- stages.traverse_(s =>
        IO.println(f"  ${s.name}%-40s ${s.perSecond}%,12.0f /s ${s.microsPerItem}%9.2f us/item")
      )
    yield ExitCode.Success
