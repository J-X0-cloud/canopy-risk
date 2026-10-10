package canopy.ingest

import canopy.TestData
import canopy.codec.JsonCodecs.given
import canopy.domain.*
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax.*

class EventParserSuite extends munit.FunSuite:
  private val valid = """{
    "id": "t-1", "timestamp": "2026-03-02T12:00:00Z", "account_id": "a", "card_id": "c",
    "merchant_id": "m", "mcc": 5411, "amount": "19.99", "currency": "usd", "channel": "ecommerce",
    "device_id": "d", "ip": "203.0.113.9", "ip_country": "gb", "billing_country": "US"
  }"""

  test("a valid payload decodes with normalised fields") {
    val tx = EventParser.parseLine(valid, EventParser.transaction).fold(e => fail(e.toList.mkString), identity)
    assertEquals(tx.amount, Money(1999, Currency.USD))
    assertEquals(tx.ipCountry.map(_.value), Some("GB"))
    assertEquals(tx.ip.map(_.network), Some("203.0.113.0/24"))
  }

  test("numeric amounts are accepted too") {
    val json = parse(valid).toOption.get.deepMerge(Json.obj("amount" -> Json.fromDoubleOrNull(19.5)))
    assertEquals(EventParser.transaction(json).toOption.map(_.amount.minorUnits), Some(1950L))
  }

  test("every problem is reported at once") {
    val json = Json.obj(
      "id" -> "".asJson,
      "timestamp" -> "yesterday".asJson,
      "mcc" -> 123456.asJson,
      "amount" -> "1.234".asJson,
      "currency" -> "USD".asJson,
      "channel" -> "fax".asJson,
      "ip" -> "999.1.1.1".asJson,
      "billing_country" -> "USA".asJson
    )
    val errors = EventParser.transaction(json).toEither.swap.toOption.get.toList
    val fields = errors.map(_.takeWhile(_ != ':')).toSet
    assertEquals(
      fields,
      Set("id", "timestamp", "account_id", "card_id", "merchant_id", "mcc", "amount", "channel", "ip", "billing_country")
    )
  }

  test("optional fields may be absent or null") {
    val json = parse(valid).toOption.get.deepMerge(Json.obj("device_id" -> Json.Null)).mapObject(_.remove("ip").remove("ip_country"))
    val tx = EventParser.transaction(json).toOption.get
    assertEquals((tx.deviceId, tx.ip, tx.ipCountry), (None, None, None))
  }

  test("non-object and malformed input") {
    assert(EventParser.transaction(Json.arr()).isInvalid)
    assert(EventParser.parseLine("{nope", EventParser.transaction).left.exists(_.head.startsWith("invalid JSON")))
  }

  test("transaction JSON round-trips through the encoder and parser") {
    val tx = TestData.tx("rt", amount = "1500", currency = Currency.JPY, ip = Some("2001:db8::1"))
    assertEquals(EventParser.transaction(tx.asJson).toOption, Some(tx))
  }

  test("labels decode and validate") {
    val l = Label("t", "c", Outcome.Fraud, TestData.t0)
    assertEquals(EventParser.label(l.asJson).toOption, Some(l))
    assert(EventParser.label(Json.obj("outcome" -> "maybe".asJson)).isInvalid)
  }
