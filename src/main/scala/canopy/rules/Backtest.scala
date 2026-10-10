package canopy.rules

import canopy.features.FeatureVector

/**
 * Measures each rule against labelled history before it goes live: how often it fires, how many of
 * those firings were fraud, and how much fraud it catches that no higher-priority rule already
 * caught. The incremental column is what tells an analyst whether a new rule earns its keep.
 */
object Backtest:
  final case class RuleStats(
      rule: String,
      action: String,
      hits: Int,
      fraudHits: Int,
      incrementalFraud: Int,
      unknown: Int
  ):
    def precision: Double = if hits == 0 then 0.0 else fraudHits.toDouble / hits

  final case class Report(rows: Int, fraud: Int, rules: Vector[RuleStats], caughtByAny: Int):
    def recall(stats: RuleStats): Double =
      if fraud == 0 then 0.0 else stats.fraudHits.toDouble / fraud

    def render: String =
      val header =
        f"${"rule"}%-28s ${"action"}%-12s ${"hits"}%8s ${"fraud"}%7s ${"prec"}%7s ${"recall"}%7s ${"incr"}%6s ${"unknown"}%8s"
      val lines = rules.map { s =>
        f"${s.rule}%-28s ${s.action}%-12s ${s.hits}%8d ${s.fraudHits}%7d ${100 * s.precision}%6.1f%% ${100 * recall(s)}%6.1f%% ${s.incrementalFraud}%6d ${s.unknown}%8d"
      }
      val total = if fraud == 0 then 0.0 else 100.0 * caughtByAny / fraud
      (header +: lines :+ f"rows=$rows fraud=$fraud caught by any rule=$caughtByAny ($total%.1f%%)").mkString(
        "\n"
      )

  /**
   * @param rows
   *   feature vectors with their eventual labels (1 = fraud)
   */
  def run(ruleSet: RuleSet, rows: Iterable[(FeatureVector, Int)]): Report =
    val n = ruleSet.rules.size
    val hits = Array.ofDim[Int](n)
    val fraudHits = Array.ofDim[Int](n)
    val incremental = Array.ofDim[Int](n)
    val unknown = Array.ofDim[Int](n)
    var total = 0
    var fraud = 0
    var caught = 0
    rows.foreach { (fv, label) =>
      total += 1
      if label == 1 then fraud += 1
      var alreadyCaught = false
      var i = 0
      while i < n do
        ruleSet.rules(i).condition(fv) match
          case Value.VBool(true) =>
            hits(i) += 1
            if label == 1 then
              fraudHits(i) += 1
              if !alreadyCaught then incremental(i) += 1
              alreadyCaught = true
          case Value.Missing => unknown(i) += 1
          case _             => ()
        i += 1
      if alreadyCaught then caught += 1
    }
    val stats = ruleSet.rules.indices.map { i =>
      val r = ruleSet.rules(i)
      RuleStats(r.name, r.action.render, hits(i), fraudHits(i), incremental(i), unknown(i))
    }.toVector
    Report(total, fraud, stats, caught)
