package canopy.domain

/** Final verdict, ordered by severity so the strictest of several candidate verdicts can be taken. */
enum Verdict(val wire: String, val severity: Int):
  case Approve extends Verdict("approve", 0)
  case Review extends Verdict("review", 1)
  case Decline extends Verdict("decline", 2)

object Verdict:
  def parse(s: String): Either[String, Verdict] =
    values.find(_.wire == s.trim.toLowerCase).toRight(s"unknown verdict '$s'")

  given Ordering[Verdict] = Ordering.by(_.severity)

/** A single feature's push on the model logit, in standardized units times the coefficient. */
final case class Contribution(feature: String, value: Option[Double], contribution: Double)

/** A rule that matched and what it asked for. */
final case class RuleHit(rule: String, action: String, reason: String)

/** Why a decision came out the way it did, for analysts and for the customer-facing reason code. */
final case class Explanation(
    summary: String,
    topRiskFactors: List[Contribution],
    topMitigatingFactors: List[Contribution],
    ruleHits: List[RuleHit]
)

final case class Decision(
    transactionId: String,
    verdict: Verdict,
    modelScore: Double,
    finalScore: Double,
    decidedBy: String,
    explanation: Explanation,
    tags: List[String]
)
