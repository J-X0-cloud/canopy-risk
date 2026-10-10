package canopy.features

import scala.annotation.tailrec

/**
 * A time-ordered window of observations for one entity, retained for `horizonMillis`.
 *
 * Each entry stores a running cumulative sum so the total over any suffix of the window is a
 * subtraction rather than a scan. Entries are appended in timestamp order; an event that arrives
 * out of order (but inside the horizon) is spliced into place and the cumulative sums after it are
 * rebuilt. Events older than the horizon relative to the newest entry are rejected as too late.
 */
final case class EventWindow private (
    horizonMillis: Long,
    entries: Vector[EventWindow.Entry],
    evictedSum: Double
):
  import EventWindow.Entry

  def isEmpty: Boolean = entries.isEmpty

  def size: Int = entries.size

  def newest: Option[Long] = entries.lastOption.map(_.ts)

  def oldest: Option[Long] = entries.headOption.map(_.ts)

  /** Adds an observation; returns the window unchanged if the event is beyond the horizon. */
  def add(ts: Long, value: Double, key: String): EventWindow =
    newest match
      case Some(latest) if ts < latest - horizonMillis => this
      case Some(latest) if ts < latest =>
        val idx = EventWindow.upperBound(entries, ts)
        val (before, after) = entries.splitAt(idx)
        val base = before.lastOption.fold(evictedSum)(_.cum)
        val inserted = Entry(ts, value, key, base + value)
        val rebuilt = after.scanLeft(inserted)((prev, e) => e.copy(cum = prev.cum + e.value))
        copy(entries = before ++ rebuilt)
      case _ =>
        val base = entries.lastOption.fold(evictedSum)(_.cum)
        copy(entries = entries :+ Entry(ts, value, key, base + value)).evict(ts)

  /** Drops entries that have fallen out of the horizon as of `now`. */
  def evict(now: Long): EventWindow =
    val cutoff = now - horizonMillis
    val drop = EventWindow.lowerBoundExclusive(entries, cutoff)
    if drop == 0 then this
    else copy(entries = entries.drop(drop), evictedSum = entries(drop - 1).cum)

  private def startIndex(now: Long, widthMillis: Long): Int =
    EventWindow.lowerBoundExclusive(entries, now - widthMillis)

  private def endIndex(now: Long): Int = EventWindow.upperBound(entries, now)

  /** Number of observations in (now - width, now]. */
  def count(now: Long, widthMillis: Long): Int =
    math.max(0, endIndex(now) - startIndex(now, widthMillis))

  /** Sum of values in (now - width, now], computed from cumulative sums. */
  def sum(now: Long, widthMillis: Long): Double =
    val from = startIndex(now, widthMillis)
    val to = endIndex(now)
    if to <= from then 0.0
    else
      val upper = entries(to - 1).cum
      val lower = if from == 0 then evictedSum else entries(from - 1).cum
      upper - lower

  /** Number of observations in (now - width, now] whose value satisfies `p`. */
  def countWhere(now: Long, widthMillis: Long)(p: Double => Boolean): Int =
    slice(now, widthMillis).count(e => p(e.value))

  /** Number of distinct keys in (now - width, now]. */
  def distinctKeys(now: Long, widthMillis: Long): Int =
    slice(now, widthMillis).iterator.map(_.key).toSet.size

  /** Timestamp of the latest entry strictly before `ts`. */
  def lastBefore(ts: Long): Option[Long] =
    val idx = EventWindow.lowerBound(entries, ts)
    if idx == 0 then None else Some(entries(idx - 1).ts)

  private def slice(now: Long, widthMillis: Long): Vector[Entry] =
    entries.slice(startIndex(now, widthMillis), endIndex(now))

object EventWindow:
  final case class Entry(ts: Long, value: Double, key: String, cum: Double)

  def empty(horizonMillis: Long): EventWindow =
    require(horizonMillis > 0, "horizon must be positive")
    EventWindow(horizonMillis, Vector.empty, 0.0)

  /** First index whose ts is >= target. */
  private[features] def lowerBound(xs: Vector[Entry], target: Long): Int =
    search(xs, e => e.ts >= target)

  /** First index whose ts is > target. */
  private[features] def upperBound(xs: Vector[Entry], target: Long): Int =
    search(xs, e => e.ts > target)

  /** First index whose ts is strictly greater than `cutoff` (entries at the cutoff are out). */
  private[features] def lowerBoundExclusive(xs: Vector[Entry], cutoff: Long): Int =
    upperBound(xs, cutoff)

  /** Binary search for the first index satisfying a monotone predicate. */
  private def search(xs: Vector[Entry], p: Entry => Boolean): Int =
    @tailrec def go(lo: Int, hi: Int): Int =
      if lo >= hi then lo
      else
        val mid = (lo + hi) >>> 1
        if p(xs(mid)) then go(lo, mid) else go(mid + 1, hi)
    go(0, xs.size)
