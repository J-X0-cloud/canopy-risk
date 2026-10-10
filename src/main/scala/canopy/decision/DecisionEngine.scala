package canopy.decision

import canopy.domain.*
import canopy.ml.{LogisticRegression, Score, Thresholds}
import canopy.rules.{Action, Firing}

/** Tunables for turning scores and rule firings into verdicts. */
final case class Policy(
    thresholds: Thresholds,
    maxFactors: Int = 4,
    minContribution: Double = 0.05,
    /** A hard approve rule cannot override a model score at or above this probability. */
    approveCeiling: Double = 0.98
)

/**
 * Combines the model score with rule firings.
 *
 * The model verdict comes from the probability after rule score adjustments, cut at the review and
 * decline thresholds. Hard rule actions are then resolved by taking the highest-priority group of
 * firings and, within it, the most severe action, so a tie between "approve" and "decline" at equal
 * priority declines.
 *
 * Review and decline rules only ever escalate: a review rule never downgrades a transaction the
 * model would decline. Approve rules are the one way to de-escalate (allow-lists for known-good
 * customers), and they are capped by `approveCeiling`.
 *
 * Score adjustments are applied in log-odds space so several adjustments compose additively and the
 * result is always a valid probability. An approve rule is ignored when the adjusted model
 * probability is at or above `approveCeiling`; allow-lists should not wave through the most obvious
 * fraud.
 */
object DecisionEngine:
  def decide(transactionId: String, score: Score, firings: List[Firing], policy: Policy): Decision =
    val adjustment = firings.collect { case Firing(_, _, Action.Score(d), _, _) => d }.sum
    val finalLogit = score.logit + adjustment
    val finalScore = LogisticRegression.sigmoid(finalLogit)
    val thresholds = policy.thresholds

    val modelVerdict =
      if finalScore >= thresholds.decline then Verdict.Decline
      else if finalScore >= thresholds.review then Verdict.Review
      else Verdict.Approve

    val hard = firings.filter(_.action.isHard)
    val topGroup = hard.headOption.map(h => hard.filter(_.priority == h.priority)).getOrElse(Nil)
    val ruleChoice = topGroup.maxByOption(f => verdictOf(f.action).severity)
    val effectiveRule = ruleChoice.filter { f =>
      f.action match
        case Action.Approve => finalScore < policy.approveCeiling
        case other          => verdictOf(other).severity >= modelVerdict.severity
    }

    val (verdict, decidedBy) = effectiveRule match
      case Some(f) => (verdictOf(f.action), s"rule:${f.rule}")
      case None    => (modelVerdict, "model")

    val risk = score.contributions
      .filter(_.contribution >= policy.minContribution)
      .sortBy(-_.contribution)
      .take(policy.maxFactors)
      .toList
    val mitigating = score.contributions
      .filter(_.contribution <= -policy.minContribution)
      .sortBy(_.contribution)
      .take(policy.maxFactors)
      .toList

    val summary = effectiveRule match
      case Some(f) =>
        val overridden =
          if verdictOf(f.action) != modelVerdict then s" (model alone would ${modelVerdict.wire})"
          else ""
        s"${pastTense(verdict)} by rule ${f.rule}: ${f.reason}$overridden"
      case None =>
        val factors =
          if risk.isEmpty then "" else s"; main risk factors: ${risk.map(_.feature).mkString(", ")}"
        val cut = verdict match
          case Verdict.Decline => s"at or above the decline threshold ${sig(thresholds.decline)}"
          case Verdict.Review =>
            s"between the review ${sig(thresholds.review)} and decline ${sig(thresholds.decline)} thresholds"
          case Verdict.Approve => s"below the review threshold ${sig(thresholds.review)}"
        val ignored = ruleChoice.filter(_ => effectiveRule.isEmpty).fold("") { f =>
          if f.action == Action.Approve then
            s"; approve rule ${f.rule} ignored because the score exceeds the ceiling"
          else s"; rule ${f.rule} asked only for ${verdictOf(f.action).wire}"
        }
        s"${pastTense(verdict)}: risk score ${sig(finalScore)} is $cut$factors$ignored"

    Decision(
      transactionId = transactionId,
      verdict = verdict,
      modelScore = score.probability,
      finalScore = finalScore,
      decidedBy = decidedBy,
      explanation = Explanation(
        summary = summary,
        topRiskFactors = risk,
        topMitigatingFactors = mitigating,
        ruleHits = firings.map(f => RuleHit(f.rule, f.action.render, f.reason))
      ),
      tags = firings.flatMap(_.tags).distinct
    )

  /** Three significant digits, so 0.000166 and 0.0849 both stay readable next to each other. */
  private[decision] def sig(p: Double): String =
    java.math.BigDecimal(p).round(java.math.MathContext(3)).stripTrailingZeros.toPlainString

  private def verdictOf(action: Action): Verdict = action match
    case Action.Approve  => Verdict.Approve
    case Action.Review   => Verdict.Review
    case Action.Decline  => Verdict.Decline
    case Action.Score(_) => Verdict.Approve

  private def pastTense(v: Verdict): String = v match
    case Verdict.Approve => "Approved"
    case Verdict.Review  => "Sent to review"
    case Verdict.Decline => "Declined"
