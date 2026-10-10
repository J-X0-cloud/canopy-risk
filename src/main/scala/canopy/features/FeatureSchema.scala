package canopy.features

/**
 * The catalogue of features the extractor produces. Rules are type checked against it and the
 * model may only reference numeric or boolean entries.
 */
object FeatureSchema:
  final case class Spec(name: String, tpe: FeatureType, description: String)

  import FeatureType.*

  val all: List[Spec] = List(
    Spec("amount", Number, "Transaction amount in its own currency, major units"),
    Spec("amount_usd", Number, "Transaction amount converted to USD"),
    Spec("log_amount_usd", Number, "ln(1 + amount_usd)"),
    Spec("currency", Text, "ISO 4217 currency code"),
    Spec("channel", Text, "pos, ecommerce, in_app or moto"),
    Spec("card_not_present", Boolean, "True for any channel other than pos"),
    Spec("hour_of_day", Number, "UTC hour of the transaction, 0-23"),
    Spec("is_night", Boolean, "UTC hour between 0 and 5 inclusive"),
    Spec("billing_country", Text, "Card billing country"),
    Spec("ip_country", Text, "Geolocated IP country, if an IP was supplied"),
    Spec("geo_mismatch", Boolean, "IP country differs from billing country"),
    Spec("merchant.mcc", Number, "Merchant category code"),
    Spec("merchant.high_risk", Boolean, "MCC is in the high-risk category list"),
    Spec("card.count_10m", Number, "Authorizations on the card in the last 10 minutes"),
    Spec("card.count_1h", Number, "Authorizations on the card in the last hour"),
    Spec("card.count_24h", Number, "Authorizations on the card in the last 24 hours"),
    Spec("card.sum_usd_1h", Number, "USD volume on the card in the last hour"),
    Spec("card.sum_usd_24h", Number, "USD volume on the card in the last 24 hours"),
    Spec("card.small_count_1h", Number, "Authorizations under 5 USD on the card in the last hour"),
    Spec("card.distinct_merchants_24h", Number, "Distinct merchants on the card in 24 hours"),
    Spec("card.distinct_countries_24h", Number, "Distinct IP countries on the card in 24 hours"),
    Spec("card.amount_zscore", Number, "Z-score of log amount against the card's decayed history"),
    Spec("card.seconds_since_last", Number, "Seconds since the previous authorization on the card"),
    Spec("card.is_new", Boolean, "No prior history for the card"),
    Spec("card.age_days", Number, "Days since the card was first seen"),
    Spec("account.new_device", Boolean, "Device never seen before on the account"),
    Spec("account.devices_30d", Number, "Distinct devices on the account in the device memory"),
    Spec("device.distinct_cards_24h", Number, "Distinct cards on the device in 24 hours"),
    Spec("device.count_1h", Number, "Authorizations from the device in the last hour"),
    Spec("ip.distinct_cards_24h", Number, "Distinct cards from the IP network in 24 hours"),
    Spec("ip.count_1h", Number, "Authorizations from the IP network in the last hour"),
    Spec("graph.linked_cards", Number, "Cards sharing a device or IP network with this card"),
    Spec("graph.component_size", Number, "Size of the bounded linked-entity neighbourhood"),
    Spec("graph.fraud_cards", Number, "Confirmed-fraud cards in the linked neighbourhood"),
    Spec("graph.fraud_ratio", Number, "Share of linked cards confirmed as fraud"),
    Spec("graph.truncated", Boolean, "Neighbourhood search hit its node cap")
  )

  val types: Map[String, FeatureType] = all.map(s => s.name -> s.tpe).toMap

  /** Features suitable as model inputs, in a stable order. */
  val modelInputs: Vector[String] =
    all.filter(s => s.tpe != Text).map(_.name).filterNot(Set("amount", "merchant.mcc")).toVector
