package canopy.ingest

import canopy.domain.*
import cats.data.{NonEmptyList, ValidatedNel}
import cats.syntax.all.*
import io.circe.{ACursor, Json}

import java.time.Instant
import java.time.format.DateTimeParseException

/**
 * Validates inbound JSON into domain values, reporting every problem with a payload at once
 * rather than only the first, which is what API clients integrating for the first time need.
 */
object EventParser:
  type Result[A] = ValidatedNel[String, A]

  private def field(c: ACursor, name: String): Result[Json] =
    c.downField(name).focus.filterNot(_.isNull).toValidNel(s"$name: required")

  private def string(c: ACursor, name: String): Result[String] =
    field(c, name).andThen(j =>
      j.asString.filter(_.trim.nonEmpty).toValidNel(s"$name: must be a non-empty string")
    )

  private def optString(c: ACursor, name: String): Result[Option[String]] =
    c.downField(name).focus.filterNot(_.isNull) match
      case None    => None.validNel
      case Some(j) => j.asString.filter(_.trim.nonEmpty).toValidNel(s"$name: must be a non-empty string").map(Some(_))

  private def from[A](name: String)(e: Either[String, A]): Result[A] = e.leftMap(m => s"$name: $m").toValidatedNel

  private def instant(c: ACursor, name: String): Result[Instant] =
    string(c, name).andThen { s =>
      try Instant.parse(s).validNel
      catch case _: DateTimeParseException => s"$name: '$s' is not an ISO-8601 instant".invalidNel
    }

  /** Amount as a JSON string ("12.30") or number (12.3); strings avoid float rounding upstream. */
  private def amount(c: ACursor, currency: Result[Currency]): Result[Money] =
    val raw: Result[BigDecimal] = field(c, "amount").andThen { j =>
      j.asString
        .flatMap(s => scala.util.Try(BigDecimal(s.trim)).toOption)
        .orElse(j.asNumber.flatMap(_.toBigDecimal))
        .toValidNel("amount: must be a decimal number or numeric string")
    }
    (raw, currency).tupled.andThen((value, cur) => from("amount")(Money.fromMajor(value, cur)))

  def transaction(json: Json): Result[Transaction] =
    val c = json.hcursor
    if json.asObject.isEmpty then "transaction must be a JSON object".invalidNel
    else
      val currency = string(c, "currency").andThen(s => from("currency")(Currency.parse(s)))
      val mcc = field(c, "mcc").andThen { j =>
        j.asNumber.flatMap(_.toInt).filter(m => m >= 0 && m <= 9999).toValidNel("mcc: must be an integer 0-9999")
      }
      (
        string(c, "id"),
        instant(c, "timestamp"),
        string(c, "account_id"),
        string(c, "card_id"),
        string(c, "merchant_id"),
        mcc,
        amount(c, currency),
        string(c, "channel").andThen(s => from("channel")(Channel.parse(s))),
        optString(c, "device_id"),
        optString(c, "ip").andThen(_.traverse(s => from("ip")(IpAddress.parse(s)))),
        optString(c, "ip_country").andThen(_.traverse(s => from("ip_country")(CountryCode.parse(s)))),
        string(c, "billing_country").andThen(s => from("billing_country")(CountryCode.parse(s)))
      ).mapN(Transaction.apply)

  def label(json: Json): Result[Label] =
    val c = json.hcursor
    (
      string(c, "transaction_id"),
      string(c, "card_id"),
      string(c, "outcome").andThen(s => from("outcome")(Outcome.parse(s))),
      instant(c, "reported_at")
    ).mapN(Label.apply)

  def parseLine[A](line: String, decode: Json => Result[A]): Either[NonEmptyList[String], A] =
    io.circe.parser.parse(line).leftMap(e => NonEmptyList.one(s"invalid JSON: ${e.message}")).flatMap(decode(_).toEither)
