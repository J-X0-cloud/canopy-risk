package canopy.domain

import scala.math.BigDecimal.RoundingMode

/** ISO 4217 currencies the engine understands, with their minor-unit exponent. */
enum Currency(val code: String, val exponent: Int):
  case USD extends Currency("USD", 2)
  case EUR extends Currency("EUR", 2)
  case GBP extends Currency("GBP", 2)
  case CAD extends Currency("CAD", 2)
  case AUD extends Currency("AUD", 2)
  case JPY extends Currency("JPY", 0)
  case KRW extends Currency("KRW", 0)
  case BHD extends Currency("BHD", 3)

object Currency:
  private val byCode: Map[String, Currency] = values.map(c => c.code -> c).toMap

  def parse(code: String): Either[String, Currency] =
    byCode.get(code.trim.toUpperCase).toRight(s"unsupported currency '$code'")

/**
 * A monetary amount held as an exact count of minor units (cents, yen, fils).
 *
 * Floating point is only used at the feature boundary, never for storage or comparison, so two
 * amounts parsed from the same decimal string always compare equal.
 */
final case class Money(minorUnits: Long, currency: Currency):
  require(minorUnits >= 0, s"money cannot be negative: $minorUnits")

  def toMajor: BigDecimal = BigDecimal(minorUnits, currency.exponent)

  /** Converts to US dollars using a static reference rate table. */
  def inUsd(rates: FxRates): Double = toMajor.toDouble * rates.toUsd(currency)

  def render: String = s"${toMajor.setScale(currency.exponent).toString} ${currency.code}"

object Money:
  /**
   * Parses a decimal major-unit amount. Rejects negatives, NaN-like input and amounts with more
   * fractional digits than the currency allows (e.g. "10.001 USD"), rather than silently rounding.
   */
  def fromMajor(amount: BigDecimal, currency: Currency): Either[String, Money] =
    if amount.signum < 0 then Left(s"amount must not be negative: $amount")
    else
      val scaled = amount.bigDecimal.movePointRight(currency.exponent)
      val rounded = BigDecimal(scaled).setScale(0, RoundingMode.UNNECESSARY)
      if !BigDecimal(scaled).isWhole then
        Left(s"amount $amount has more than ${currency.exponent} decimal places for ${currency.code}")
      else if !rounded.isValidLong then Left(s"amount $amount is out of range")
      else Right(Money(rounded.toLong, currency))

  def fromString(amount: String, currency: Currency): Either[String, Money] =
    try fromMajor(BigDecimal(amount.trim), currency)
    catch case _: NumberFormatException => Left(s"'$amount' is not a decimal amount")

/** Static reference FX table. Scoring only needs a stable scale, not trading-grade rates. */
final case class FxRates(usdRates: Map[Currency, Double]):
  def toUsd(currency: Currency): Double = usdRates.getOrElse(currency, 1.0)

object FxRates:
  val reference: FxRates = FxRates(
    Map(
      Currency.USD -> 1.0,
      Currency.EUR -> 1.08,
      Currency.GBP -> 1.27,
      Currency.CAD -> 0.73,
      Currency.AUD -> 0.66,
      Currency.JPY -> 0.0067,
      Currency.KRW -> 0.00073,
      Currency.BHD -> 2.65
    )
  )
