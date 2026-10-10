package canopy.ingest

import fs2.{Chunk, Pipe, Pull, Stream}

import scala.collection.immutable.{Queue, TreeMap}

/** Result of passing an event through the reorder buffer. */
enum Ordered[+A]:
  case InOrder(value: A)

  /** Arrived after events with later timestamps had already been released. */
  case Late(value: A, lagMillis: Long)

/** One problem found while reading an input line. */
final case class IngestError(line: Long, messages: List[String]):
  def render: String = s"line $line: ${messages.mkString("; ")}"

object StreamOps:
  /**
   * Splits a byte stream into NDJSON lines and decodes each one, keeping 1-based line numbers for
   * error reporting. Blank lines are skipped.
   */
  def ndjson[F[_], A](decode: String => Either[List[String], A])
      : Pipe[F, Byte, Either[IngestError, A]] =
    _.through(fs2.text.utf8.decode)
      .through(fs2.text.lines)
      .zipWithIndex
      .filter((line, _) => line.trim.nonEmpty)
      .map((line, idx) => decode(line).left.map(IngestError(idx + 1, _)))

  /**
   * Event-time reordering with bounded lateness.
   *
   * Events are buffered in timestamp order and released once the watermark (the largest timestamp
   * seen minus `maxLatenessMillis`) passes them, so anything up to `maxLatenessMillis` late is put
   * back in order. Events older than what has already been released cannot be reordered and are
   * emitted as [[Ordered.Late]] rather than silently dropped. Ties keep arrival order. The buffer
   * is flushed when the input ends.
   */
  def reorder[F[_], A](maxLatenessMillis: Long)(timestamp: A => Long): Pipe[F, A, Ordered[A]] =
    require(maxLatenessMillis >= 0, "lateness must be non-negative")
    final case class State(
        buffer: TreeMap[(Long, Long), A],
        seq: Long,
        maxSeen: Long,
        released: Long
    )

    def step(state: State, a: A): (State, Vector[Ordered[A]]) =
      val ts = timestamp(a)
      if ts < state.released then
        (state.copy(seq = state.seq + 1), Vector(Ordered.Late(a, state.released - ts)))
      else
        val buffered = state.buffer.updated((ts, state.seq), a)
        val maxSeen = math.max(state.maxSeen, ts)
        val watermark = maxSeen - maxLatenessMillis
        val (ready, keep) = buffered.span { case ((t, _), _) => t <= watermark }
        val released = ready.lastOption.fold(state.released)(_._1._1)
        (
          State(keep, state.seq + 1, maxSeen, released),
          ready.values.map(Ordered.InOrder(_)).toVector
        )

    def go(s: Stream[F, A], state: State): Pull[F, Ordered[A], Unit] =
      s.pull.uncons.flatMap {
        case Some((chunk, rest)) =>
          val (next, out) =
            chunk.foldLeft((state, Vector.empty[Ordered[A]])) { case ((st, acc), a) =>
              val (st2, emitted) = step(st, a)
              (st2, acc ++ emitted)
            }
          Pull.output(Chunk.from(out)) >> go(rest, next)
        case None => Pull.output(Chunk.from(state.buffer.values.map(Ordered.InOrder(_))))
      }

    in => go(in, State(TreeMap.empty, 0L, Long.MinValue, Long.MinValue)).stream

  /**
   * Drops events whose key was already seen within `windowMillis` of event time. Memory is bounded
   * by the number of distinct keys inside the window: keys are expired in arrival order as the
   * clock advances.
   */
  def dedupe[F[_], A](windowMillis: Long)(key: A => String, timestamp: A => Long): Pipe[F, A, A] =
    final case class State(seen: Map[String, Long], expiry: Queue[(Long, String)], clock: Long)

    def expire(state: State): State =
      val cutoff = state.clock - windowMillis
      @scala.annotation.tailrec
      def loop(
          seen: Map[String, Long],
          q: Queue[(Long, String)]
      ): (Map[String, Long], Queue[(Long, String)]) =
        q.dequeueOption match
          case Some(((ts, k), rest)) if ts < cutoff =>
            // Only forget the key if this queue entry is its latest sighting.
            loop(if seen.get(k).contains(ts) then seen - k else seen, rest)
          case _ => (seen, q)
      val (seen, q) = loop(state.seen, state.expiry)
      state.copy(seen = seen, expiry = q)

    _.mapAccumulate(State(Map.empty, Queue.empty, Long.MinValue)) { (state, a) =>
      val ts = timestamp(a)
      val k = key(a)
      val advanced = expire(state.copy(clock = math.max(state.clock, ts)))
      if advanced.seen.contains(k) then (advanced, None)
      else
        (
          advanced.copy(
            seen = advanced.seen.updated(k, ts),
            expiry = advanced.expiry.enqueue((ts, k))
          ),
          Some(a)
        )
    }.collect { case (_, Some(a)) => a }
