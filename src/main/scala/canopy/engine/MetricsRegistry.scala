package canopy.engine

import canopy.domain.{Decision, Verdict}
import cats.effect.{IO, Ref}

/** A fixed-bucket latency histogram in seconds, Prometheus style (cumulative on render). */
final case class Histogram(bounds: Vector[Double], counts: Vector[Long], sum: Double, count: Long):
  def observe(v: Double): Histogram =
    val idx = bounds.indexWhere(v <= _)
    val slot = if idx < 0 then bounds.size else idx
    copy(counts = counts.updated(slot, counts(slot) + 1), sum = sum + v, count = count + 1)

  /** Upper bound of the bucket containing the q-quantile (bucket resolution, not interpolated). */
  def quantile(q: Double): Option[Double] =
    if count == 0 then None
    else
      val target = math.ceil(q * count).toLong.max(1)
      val cumulative = counts.scanLeft(0L)(_ + _).tail
      val idx = cumulative.indexWhere(_ >= target)
      Some(if idx < bounds.size then bounds(idx) else Double.PositiveInfinity)

object Histogram:
  val defaultBounds: Vector[Double] =
    Vector(0.0001, 0.00025, 0.0005, 0.001, 0.0025, 0.005, 0.01, 0.025, 0.05, 0.1, 0.25)

  def empty(bounds: Vector[Double] = defaultBounds): Histogram =
    require(
      bounds == bounds.sorted && bounds.distinct == bounds,
      "bounds must be strictly increasing"
    )
    Histogram(bounds, Vector.fill(bounds.size + 1)(0L), 0.0, 0L)

final case class MetricsSnapshot(
    decisions: Map[Verdict, Long],
    ruleHits: Map[String, Long],
    labels: Long,
    rejected: Long,
    duplicates: Long,
    latency: Histogram
)

/** In-process counters exposed on /metrics in the Prometheus text format. */
final class MetricsRegistry private (ref: Ref[IO, MetricsSnapshot]):
  def recordDecision(d: Decision, seconds: Double): IO[Unit] = ref.update { s =>
    s.copy(
      decisions = s.decisions.updatedWith(d.verdict)(c => Some(c.getOrElse(0L) + 1)),
      ruleHits = d.explanation.ruleHits.foldLeft(s.ruleHits)((m, h) =>
        m.updatedWith(h.rule)(c => Some(c.getOrElse(0L) + 1))
      ),
      latency = s.latency.observe(seconds)
    )
  }

  def recordLabel: IO[Unit] = ref.update(s => s.copy(labels = s.labels + 1))
  def recordRejected: IO[Unit] = ref.update(s => s.copy(rejected = s.rejected + 1))
  def recordDuplicate: IO[Unit] = ref.update(s => s.copy(duplicates = s.duplicates + 1))

  def snapshot: IO[MetricsSnapshot] = ref.get

  def render(extraGauges: List[(String, String, Double)]): IO[String] = ref.get.map { s =>
    val sb = StringBuilder()
    def line(str: String): Unit =
      sb.append(str).append('\n'); ()
    def num(d: Double): String =
      if d.isPosInfinity then "+Inf"
      else java.math.BigDecimal.valueOf(d).stripTrailingZeros.toPlainString
    def esc(v: String): String = v.flatMap {
      case '\\' => "\\\\"
      case '"'  => "\\\""
      case '\n' => "\\n"
      case c    => c.toString
    }

    line("# HELP canopy_decisions_total Decisions issued, by verdict.")
    line("# TYPE canopy_decisions_total counter")
    Verdict.values.foreach(v =>
      line(s"""canopy_decisions_total{verdict="${v.wire}"} ${s.decisions.getOrElse(v, 0L)}""")
    )
    line("# HELP canopy_rule_hits_total Rule firings, by rule.")
    line("# TYPE canopy_rule_hits_total counter")
    s.ruleHits.toList.sortBy(_._1).foreach((r, c) =>
      line(s"""canopy_rule_hits_total{rule="${esc(r)}"} $c""")
    )
    line("# HELP canopy_labels_total Outcome labels received.")
    line("# TYPE canopy_labels_total counter")
    line(s"canopy_labels_total ${s.labels}")
    line("# HELP canopy_rejected_events_total Events rejected by validation.")
    line("# TYPE canopy_rejected_events_total counter")
    line(s"canopy_rejected_events_total ${s.rejected}")
    line("# HELP canopy_duplicate_events_total Repeated transaction ids answered from the cache.")
    line("# TYPE canopy_duplicate_events_total counter")
    line(s"canopy_duplicate_events_total ${s.duplicates}")
    line("# HELP canopy_scoring_seconds Time to score one transaction.")
    line("# TYPE canopy_scoring_seconds histogram")
    val cumulative = s.latency.counts.scanLeft(0L)(_ + _).tail
    (s.latency.bounds :+ Double.PositiveInfinity).zip(cumulative).foreach { (b, c) =>
      line(s"""canopy_scoring_seconds_bucket{le="${num(b)}"} $c""")
    }
    line(s"canopy_scoring_seconds_sum ${s.latency.sum}")
    line(s"canopy_scoring_seconds_count ${s.latency.count}")
    extraGauges.foreach { (name, help, value) =>
      line(s"# HELP $name $help")
      line(s"# TYPE $name gauge")
      line(s"$name ${num(value)}")
    }
    sb.result()
  }

object MetricsRegistry:
  def create: IO[MetricsRegistry] =
    Ref.of[IO, MetricsSnapshot](MetricsSnapshot(
      Map.empty,
      Map.empty,
      0,
      0,
      0,
      Histogram.empty()
    )).map(MetricsRegistry(_))
