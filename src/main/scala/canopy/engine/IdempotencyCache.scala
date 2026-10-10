package canopy.engine

import scala.collection.immutable.Queue

/**
 * Remembers the last `capacity` results by key, evicting in insertion order. Payment clients retry
 * on timeouts; answering a retried transaction id from the cache keeps the decision stable and
 * stops the retry from being counted twice in velocity features.
 */
final case class IdempotencyCache[A](capacity: Int, entries: Map[String, A], order: Queue[String]):
  def get(key: String): Option[A] = entries.get(key)

  def put(key: String, value: A): IdempotencyCache[A] =
    if entries.contains(key) then copy(entries = entries.updated(key, value))
    else
      val (e, o) =
        if entries.size < capacity then (entries, order)
        else
          order.dequeueOption match
            case Some((oldest, rest)) => (entries - oldest, rest)
            case None                 => (entries, order)
      IdempotencyCache(capacity, e.updated(key, value), o.enqueue(key))

  def size: Int = entries.size

object IdempotencyCache:
  def empty[A](capacity: Int): IdempotencyCache[A] =
    require(capacity > 0, "capacity must be positive")
    IdempotencyCache(capacity, Map.empty, Queue.empty)
