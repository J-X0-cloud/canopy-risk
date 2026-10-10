package canopy.features

/** Windowing and memory parameters for the feature extractor. */
final case class FeatureConfig(
    windowHorizonMillis: Long = FeatureConfig.Day,
    amountHalfLifeMillis: Long = 14 * FeatureConfig.Day,
    deviceMemory: Int = 16,
    deviceMemoryMillis: Long = 30 * FeatureConfig.Day,
    idleRetentionMillis: Long = 45 * FeatureConfig.Day,
    smallAmountUsd: Double = 5.0,
    highRiskMccs: Set[Int] = FeatureConfig.defaultHighRiskMccs
)

object FeatureConfig:
  val Minute: Long = 60_000L
  val Hour: Long = 60 * Minute
  val Day: Long = 24 * Hour

  /**
   * Gambling, quasi-cash, money transfer, crypto and similar categories that card networks classify
   * as elevated risk.
   */
  val defaultHighRiskMccs: Set[Int] = Set(4829, 5967, 6051, 6211, 6540, 7273, 7995)

final case class CardState(
    activity: EventWindow,
    countries: EventWindow,
    amounts: DecayingStats,
    firstSeen: Long,
    lastSeen: Long
)

final case class AccountState(devices: LruSet, lastSeen: Long)

final case class SharedState(cards: EventWindow, lastSeen: Long)

/** Every per-entity aggregate the extractor maintains. Immutable; updated per event. */
final case class FeatureState(
    cards: Map[String, CardState],
    accounts: Map[String, AccountState],
    devices: Map[String, SharedState],
    networks: Map[String, SharedState]
):
  def entityCount: Int = cards.size + accounts.size + devices.size + networks.size

  /** Removes every entity that has been idle for longer than the retention period. */
  def evictIdle(now: Long, retentionMillis: Long): FeatureState =
    val cutoff = now - retentionMillis
    FeatureState(
      cards.filter(_._2.lastSeen >= cutoff),
      accounts.filter(_._2.lastSeen >= cutoff),
      devices.filter(_._2.lastSeen >= cutoff),
      networks.filter(_._2.lastSeen >= cutoff)
    )

object FeatureState:
  val empty: FeatureState = FeatureState(Map.empty, Map.empty, Map.empty, Map.empty)
