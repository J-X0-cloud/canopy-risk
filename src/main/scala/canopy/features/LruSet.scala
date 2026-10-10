package canopy.features

/**
 * A bounded set of keys with last-seen timestamps. When full, the least recently seen key is
 * dropped. Used for "devices this account has used" style memory where unbounded growth would let
 * one noisy account exhaust the heap.
 */
final case class LruSet(capacity: Int, entries: Map[String, Long]):
  def contains(key: String): Boolean = entries.contains(key)

  def size: Int = entries.size

  def touch(key: String, ts: Long): LruSet =
    val updated = entries.updated(key, math.max(ts, entries.getOrElse(key, Long.MinValue)))
    if updated.size <= capacity then copy(entries = updated)
    else copy(entries = updated - updated.minBy(_._2)._1)

  def expire(cutoff: Long): LruSet = copy(entries = entries.filter(_._2 >= cutoff))

object LruSet:
  def empty(capacity: Int): LruSet =
    require(capacity > 0, "capacity must be positive")
    LruSet(capacity, Map.empty)
