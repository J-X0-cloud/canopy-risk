package canopy.engine

import canopy.decision.{DecisionEngine, Policy}
import canopy.domain.{Decision, Label, Transaction}
import canopy.features.FeatureVector
import canopy.ml.LogisticModel
import canopy.rules.{Diagnostic, RuleSet}
import cats.effect.std.AtomicCell
import cats.effect.{Clock, IO, Ref}

/** The model, rules and policy currently in force. Swapped atomically on reload. */
final case class Configuration(model: LogisticModel, rules: RuleSet, policy: Policy)

/** Engine state plus the idempotency cache, updated together so retries cannot race. */
final case class LiveState(engine: EngineState, recent: IdempotencyCache[Decision])

final case class ServiceStats(processed: Long, entities: Int, graphNodes: Int, graphEdges: Int, cached: Int)

/**
 * The stateful heart of the engine.
 *
 * State updates go through an [[AtomicCell]], so each transaction's feature computation runs
 * exactly once and in a total order, which the velocity windows require. Model, rules and policy
 * live in a separate `Ref` so a rules hot-reload never waits behind scoring.
 */
final class ScoringService private (
    pipeline: FeaturePipeline,
    state: AtomicCell[IO, LiveState],
    configuration: Ref[IO, Configuration],
    val metrics: MetricsRegistry
):
  private def evaluate(tx: Transaction, fv: FeatureVector, conf: Configuration): Decision =
    val score = conf.model.score(fv)
    val firings = conf.rules.evaluate(fv)
    DecisionEngine.decide(tx.id, score, firings, conf.policy)

  /** Scores and commits the transaction to state. Retried ids are answered from the cache. */
  def score(tx: Transaction): IO[Decision] =
    for
      start <- Clock[IO].monotonic
      conf <- configuration.get
      outcome <- state.evalModify { live =>
        IO {
          live.recent.get(tx.id) match
            case Some(previous) => (live, (previous, true))
            case None =>
              val (next, fv) = pipeline.observe(live.engine, tx)
              val decision = evaluate(tx, fv, conf)
              (LiveState(next, live.recent.put(tx.id, decision)), (decision, false))
        }
      }
      (decision, duplicate) = outcome
      end <- Clock[IO].monotonic
      _ <-
        if duplicate then metrics.recordDuplicate
        else metrics.recordDecision(decision, (end - start).toNanos / 1e9)
    yield decision

  /** Scores against current state without recording the transaction. */
  def preview(tx: Transaction): IO[Decision] =
    for
      conf <- configuration.get
      live <- state.get
    yield evaluate(tx, pipeline.peek(live.engine, tx), conf)

  /** Feature vector for a transaction without recording it, for debugging rules. */
  def features(tx: Transaction): IO[FeatureVector] = state.get.map(live => pipeline.peek(live.engine, tx))

  def label(l: Label): IO[Unit] = state.update(live => live.copy(engine = pipeline.label(live.engine, l))) *> metrics.recordLabel

  def current: IO[Configuration] = configuration.get

  def validateRules(source: String): IO[Either[List[Diagnostic], RuleSet]] = IO(RuleSet.compile(source))

  /** Compiles and, if valid, atomically installs a new rule program. */
  def replaceRules(source: String): IO[Either[List[Diagnostic], RuleSet]] =
    validateRules(source).flatTap {
      case Right(rs) => configuration.update(_.copy(rules = rs))
      case Left(_)   => IO.unit
    }

  def replaceModel(model: LogisticModel): IO[Unit] =
    configuration.update(c => c.copy(model = model, policy = c.policy.copy(thresholds = model.thresholds)))

  def stats: IO[ServiceStats] =
    state.get.map { live =>
      val s = live.engine
      ServiceStats(s.processed, s.entityCount, s.graph.nodeCount, s.graph.edgeCount, live.recent.size)
    }

object ScoringService:
  def create(
      model: LogisticModel,
      rules: RuleSet,
      pipeline: FeaturePipeline = FeaturePipeline(),
      cacheSize: Int = 100_000
  ): IO[ScoringService] =
    for
      cell <- AtomicCell[IO].of(LiveState(pipeline.emptyState, IdempotencyCache.empty(cacheSize)))
      conf <- Ref.of[IO, Configuration](Configuration(model, rules, Policy(model.thresholds)))
      metrics <- MetricsRegistry.create
    yield ScoringService(pipeline, cell, conf, metrics)
