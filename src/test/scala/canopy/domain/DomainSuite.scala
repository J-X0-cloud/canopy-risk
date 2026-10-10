package canopy.domain

class DomainSuite extends munit.FunSuite:
  test("money parses exact decimal amounts into minor units") {
    assertEquals(Money.fromString("12.34", Currency.USD), Right(Money(1234, Currency.USD)))
    assertEquals(Money.fromString("1000", Currency.JPY), Right(Money(1000, Currency.JPY)))
    assertEquals(Money.fromString("1.234", Currency.BHD), Right(Money(1234, Currency.BHD)))
    assertEquals(Money.fromString("0.1", Currency.USD), Right(Money(10, Currency.USD)))
  }

  test("money rejects excess precision, negatives and garbage instead of rounding") {
    assert(Money.fromString("1.001", Currency.USD).isLeft)
    assert(Money.fromString("1.5", Currency.JPY).isLeft)
    assert(Money.fromString("-1", Currency.USD).isLeft)
    assert(Money.fromString("abc", Currency.USD).isLeft)
    assert(Money.fromString("1e30", Currency.USD).isLeft)
  }

  test("money renders with the currency's scale and converts to USD") {
    assertEquals(Money(500, Currency.USD).render, "5.00 USD")
    assertEquals(Money(1000, Currency.JPY).inUsd(FxRates.reference), 6.7, 1e-9)
  }

  test("currency lookup is case insensitive") {
    assertEquals(Currency.parse(" eur "), Right(Currency.EUR))
    assert(Currency.parse("XXX").isLeft)
  }

  test("IPv4 parsing and /24 networks") {
    val ip = IpAddress.parse("192.168.10.200").toOption.get
    assertEquals(ip.network, "192.168.10.0/24")
    List("256.1.1.1", "1.2.3", "01.2.3.4", "1.2.3.4.5", "a.b.c.d", "1..2.3").foreach { bad =>
      assert(IpAddress.parse(bad).isLeft, bad)
    }
  }

  test("IPv6 parsing expands '::' and normalises case and leading zeros") {
    val a = IpAddress.parse("2001:DB8:0:0::1").toOption.get
    val b = IpAddress.parse("2001:db8::0001").toOption.get
    assertEquals(a, b)
    assertEquals(a.canonical, "2001:db8:0:0:0:0:0:1")
    assertEquals(a.network, "2001:db8:0::/48")
    assertEquals(IpAddress.parse("::1").map(_.canonical), Right("0:0:0:0:0:0:0:1"))
    List("1::2::3", "12345::1", "1:2:3:4:5:6:7", "1:2:3:4:5:6:7:8:9", "g::1", "1:2:3:4::5:6:7:8").foreach {
      bad => assert(IpAddress.parse(bad).isLeft, bad)
    }
  }

  test("country codes are validated and upper-cased") {
    assertEquals(CountryCode.parse("us").map(_.value), Right("US"))
    assert(CountryCode.parse("USA").isLeft)
    assert(CountryCode.parse("U1").isLeft)
  }

  test("channels and verdicts parse from their wire names") {
    assertEquals(Channel.parse("IN_APP"), Right(Channel.InApp))
    assert(!Channel.Pos.cardNotPresent)
    assert(Channel.Moto.cardNotPresent)
    assertEquals(List(Verdict.Decline, Verdict.Approve, Verdict.Review).max, Verdict.Decline)
    assertEquals(Verdict.parse("review"), Right(Verdict.Review))
  }
