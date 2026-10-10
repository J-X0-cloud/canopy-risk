package canopy.domain

import java.time.Instant

/**
 * Ground truth that arrives after the fact: chargebacks, confirmed fraud reports, cleared disputes.
 */
enum Outcome(val wire: String):
  case Fraud extends Outcome("fraud")
  case Legit extends Outcome("legit")

object Outcome:
  def parse(s: String): Either[String, Outcome] =
    values.find(_.wire == s.trim.toLowerCase).toRight(s"unknown outcome '$s'")

final case class Label(transactionId: String, cardId: String, outcome: Outcome, reportedAt: Instant)
