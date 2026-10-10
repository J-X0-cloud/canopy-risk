package canopy

import canopy.domain.*

import java.time.Instant

/** Concise transaction builders for tests. */
object TestData:
  val t0: Instant = Instant.parse("2026-03-02T12:00:00Z")

  def tx(
      id: String,
      at: Instant = t0,
      account: String = "acct-1",
      card: String = "card-1",
      merchant: String = "m-1",
      mcc: Int = 5411,
      amount: String = "25.00",
      currency: Currency = Currency.USD,
      channel: Channel = Channel.Ecommerce,
      device: Option[String] = Some("dev-1"),
      ip: Option[String] = Some("203.0.113.7"),
      ipCountry: Option[String] = Some("US"),
      billing: String = "US"
  ): Transaction =
    Transaction(
      id = id,
      timestamp = at,
      accountId = account,
      cardId = card,
      merchantId = merchant,
      mcc = mcc,
      amount = Money.fromString(amount, currency).fold(e => throw IllegalArgumentException(e), identity),
      channel = channel,
      deviceId = device,
      ip = ip.map(s => IpAddress.parse(s).fold(e => throw IllegalArgumentException(e), identity)),
      ipCountry = ipCountry.map(CountryCode.unsafe),
      billingCountry = CountryCode.unsafe(billing)
    )

  extension (i: Instant)
    def plusMinutes(m: Long): Instant = i.plusSeconds(m * 60)
    def plusHours(h: Long): Instant = i.plusSeconds(h * 3600)
