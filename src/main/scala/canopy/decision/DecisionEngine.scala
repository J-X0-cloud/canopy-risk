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
 * Precedence, from strongest to weakest:
 *   1. hard rule actions, taking the highest-priority group and, within it, the most severe
 *      verdict, so a tie between "approve" and "decline" at equal priority declines;
 *   2. the model probability after rule score adjustments, cut at the review and decline
 *      thresholds.
 *
 * Score adjustments are applied in log-odds space so several adjustments compose additively and
 * the result is always a valid probability. An approve rule is ignored when the adjusted model
 * probability is at or above `approveCeiling`; allow-lists should not wave through the most
 * obvious fraud.
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
    val effectiveRule = ruleChoice.filterNot(f => f.action == Action.Approve && finalScore >= policy.approveCeiling)

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
          if verdictOf(f.action) != modelVerdict then s" (model alone would ${modelVerdict.wire})" else ""
        s"${pastTense(verdict)} by rule ${f.rule}: ${f.reason}$overridden"
      case None =>
        val factors =
          if risk.isEmpty then "" else s"; main risk factors: ${risk.map(_.feature).mkString(", ")}"
        val cut = verdict match
          case Verdict.Decline => f"at or above the decline threshold ${thresholds.decline}%.3f"
          case Verdict.Review  => f"between the review ${thresholds.review}%.3f and decline ${thresholds.decline}%.3f thresholds"
          case Verdict.Approve => f"below the review threshold ${thresholds.review}%.3f"
        val ignored = ruleChoice.filter(_ => effectiveRule.isEmpty)
          .fold("")(f => s"; approve rule ${f.rule} ignored because the score exceeds the ceiling")
        f"${pastTense(verdict)}: risk score $finalScore%.3f is $cut$factors$ignored"

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

  private def verdictOf(action: Action): Verdict = action match
    case Action.Approve  => Verdict.Approve
    case Action.Review   => Verdict.Review
    case Action.Decline  => Verdict.Decline
    case Action.Score(_) => Verdict.Approve

  private def pastTense(v: Verdict): String = v match
    case Verdict.Approve => "Approved"
    case Verdict.Review  => "Sent to review"
    case Verdict.Decline => "Declined"
