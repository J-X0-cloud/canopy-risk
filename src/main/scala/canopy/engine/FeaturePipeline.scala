package canopy.engine

import canopy.domain.{Label, Transaction}
import canopy.features.{FeatureConfig, FeatureExtractor, FeatureState, FeatureVector}
import canopy.graph.{EntityGraph, GraphConfig}

/** All mutable-in-spirit state behind scoring, as one immutable value. */
final case class EngineState(
    features: FeatureState,
    graph: EntityGraph,
    processed: Long,
    watermark: Long
):
  def entityCount: Int = features.entityCount

object EngineState:
  def empty(graphConfig: GraphConfig = GraphConfig()): EngineState =
    EngineState(FeatureState.empty, EntityGraph.empty(graphConfig), 0L, Long.MinValue)

/**
 * Combines windowed velocity features with entity-graph features. The same pure function is used
 * when replaying history for training and when serving, which is what guarantees there is no
 * training/serving skew.
 */
final class FeaturePipeline(
    featureConfig: FeatureConfig = FeatureConfig(),
    val graphConfig: GraphConfig = GraphConfig(),
    maintenanceEvery: Long = 50_000L
):
  private val extractor = FeatureExtractor(featureConfig)

  def emptyState: EngineState = EngineState.empty(graphConfig)

  def observe(state: EngineState, tx: Transaction): (EngineState, FeatureVector) =
    val ts = tx.timestamp.toEpochMilli
    val (features, fv) = extractor.observe(state.features, tx)
    val graph = state.graph.record(tx)
    val g = graph.featuresFor(tx.cardId, ts)
    val enriched = fv
      .withNum("graph.linked_cards", g.linkedCards.toDouble)
      .withNum("graph.component_size", g.componentSize.toDouble)
      .withNum("graph.fraud_cards", g.fraudCards.toDouble)
      .withNum("graph.fraud_ratio", g.fraudRatio)
      .withBool("graph.truncated", g.truncated)
    val next = EngineState(features, graph, state.processed + 1, math.max(state.watermark, ts))
    (maintain(next), enriched)

  /** Applies a confirmed outcome to the graph so linked cards inherit (or shed) the risk. */
  def label(state: EngineState, l: Label): EngineState =
    state.copy(graph = state.graph.label(l.cardId, l.outcome, l.reportedAt.toEpochMilli))

  /** Features for a hypothetical transaction without committing it to state. */
  def peek(state: EngineState, tx: Transaction): FeatureVector = observe(state, tx)._2

  private def maintain(state: EngineState): EngineState =
    if state.processed % maintenanceEvery != 0 then state
    else
      state.copy(
        features = state.features.evictIdle(state.watermark, featureConfig.idleRetentionMillis),
        graph = state.graph.prune(state.watermark)
      )
