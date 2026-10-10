package canopy.domain

import java.time.Instant

/** How the card was presented. Card-not-present channels carry most remote fraud. */
enum Channel(val wire: String):
  case Pos extends Channel("pos")
  case Ecommerce extends Channel("ecommerce")
  case InApp extends Channel("in_app")
  case Moto extends Channel("moto")

  def cardNotPresent: Boolean = this != Channel.Pos

object Channel:
  def parse(s: String): Either[String, Channel] =
    values.find(_.wire == s.trim.toLowerCase).toRight(s"unknown channel '$s'")

/**
 * One authorization request as received from the acquiring side.
 *
 * Identifiers are opaque strings supplied by the caller (tokenised PAN, device fingerprint hash,
 * etc.). `deviceId` and `ip` are optional because POS terminals usually report neither.
 */
final case class Transaction(
    id: String,
    timestamp: Instant,
    accountId: String,
    cardId: String,
    merchantId: String,
    mcc: Int,
    amount: Money,
    channel: Channel,
    deviceId: Option[String],
    ip: Option[IpAddress],
    ipCountry: Option[CountryCode],
    billingCountry: CountryCode
)

/** Two-letter ISO 3166 country code, upper case. */
opaque type CountryCode = String

object CountryCode:
  def parse(s: String): Either[String, CountryCode] =
    val t = s.trim.toUpperCase
    if t.length == 2 && t.forall(c => c >= 'A' && c <= 'Z') then Right(t)
    else Left(s"'$s' is not a two-letter country code")

  def unsafe(s: String): CountryCode = parse(s).fold(e => throw IllegalArgumentException(e), identity)

  extension (c: CountryCode) def value: String = c

/** A validated IPv4 or IPv6 address in canonical textual form. */
final case class IpAddress private (canonical: String, isV4: Boolean):
  /**
   * The network the address belongs to: /24 for IPv4 and /48 for IPv6. Fraud rings rotate hosts
   * inside one allocation far more often than they rotate allocations.
   */
  def network: String =
    if isV4 then canonical.split('.').take(3).mkString(".") + ".0/24"
    else canonical.split(':').take(3).mkString(":") + "::/48"

object IpAddress:
  def parse(s: String): Either[String, IpAddress] =
    val t = s.trim
    if t.contains(':') then parseV6(t) else parseV4(t)

  private def parseV4(s: String): Either[String, IpAddress] =
    val parts = s.split("\\.", -1)
    val ok = parts.length == 4 && parts.forall { p =>
      p.nonEmpty && p.length <= 3 && p.forall(_.isDigit) && p.toInt <= 255 &&
      !(p.length > 1 && p.head == '0')
    }
    if ok then Right(IpAddress(s, isV4 = true)) else Left(s"'$s' is not a valid IPv4 address")

  private def parseV6(s: String): Either[String, IpAddress] =
    val doubleColon = s.indexOf("::")
    if doubleColon != s.lastIndexOf("::") then Left(s"'$s' has more than one '::'")
    else
      def groups(part: String): Option[Vector[Int]] =
        if part.isEmpty then Some(Vector.empty)
        else
          val gs = part.split(":", -1).toVector
          if gs.forall(g => g.nonEmpty && g.length <= 4 && g.forall(isHex)) then
            Some(gs.map(Integer.parseInt(_, 16)))
          else None
      val expanded: Option[Vector[Int]] =
        if doubleColon >= 0 then
          for
            head <- groups(s.substring(0, doubleColon))
            tail <- groups(s.substring(doubleColon + 2))
            if head.size + tail.size < 8
          yield head ++ Vector.fill(8 - head.size - tail.size)(0) ++ tail
        else groups(s).filter(_.size == 8)
      expanded
        .map(gs => IpAddress(gs.map(g => Integer.toHexString(g)).mkString(":"), isV4 = false))
        .toRight(s"'$s' is not a valid IPv6 address")

  private def isHex(c: Char): Boolean =
    c.isDigit || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
