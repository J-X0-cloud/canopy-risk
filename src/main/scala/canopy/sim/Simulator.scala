package canopy.sim

import canopy.domain.*

import java.time.{Duration, Instant}

/** Knobs for the synthetic data generator. Rates are per cardholder over the whole period. */
final case class SimConfig(
    seed: Long = 7L,
    cardholders: Int = 2000,
    days: Int = 30,
    start: Instant = Instant.parse("2026-01-05T00:00:00Z"),
    merchants: Int = 400,
    cardTestingAttacks: Int = 40,
    takeovers: Int = 40,
    fraudRings: Int = 6,
    unreportedFraudShare: Double = 0.1,
    travelProbability: Double = 0.03,
    newDeviceProbability: Double = 0.01
)

final case class SimResult(transactions: Vector[Transaction], labels: Vector[Label]):
  def fraudIds: Set[String] =
    labels.iterator.filter(_.outcome == Outcome.Fraud).map(_.transactionId).toSet

/**
 * Generates a labelled stream of card transactions with realistic fraud patterns.
 *
 * Legitimate behaviour: each cardholder has a home country and currency, one or two devices, a home
 * IP network, a handful of favourite merchants, a typical spend level and a daytime-heavy activity
 * rhythm. Noise is deliberate: people travel, buy new phones, and make legitimate sub-$5 purchases,
 * so no single rule separates the classes.
 *
 * Fraud behaviour:
 *   - card testing: a burst of tiny authorisations on a stolen card from a fraudster device,
 *     followed by large purchases once the card is confirmed live;
 *   - account takeover: a new device in a foreign country spending big in risky categories;
 *   - rings: a few devices and one network cycling through many stolen cards over days.
 *
 * Chargeback labels arrive days after the fact and some fraud is never reported, which mirrors the
 * label delay and label noise a production model trains under.
 */
object Simulator:
  final private case class Merchant(id: String, mcc: Int, cnp: Boolean)

  final private case class Cardholder(
      account: String,
      cards: Vector[String],
      country: CountryCode,
      currency: Currency,
      devices: Vector[String],
      network: String,
      merchants: Vector[Merchant],
      spendMu: Double,
      dailyRate: Double,
      traveler: Boolean,
      microSpender: Boolean
  )

  private val homeCountries: Vector[(String, Currency, Double)] = Vector(
    ("US", Currency.USD, 0.55),
    ("GB", Currency.GBP, 0.12),
    ("DE", Currency.EUR, 0.1),
    ("CA", Currency.CAD, 0.08),
    ("AU", Currency.AUD, 0.07),
    ("JP", Currency.JPY, 0.08)
  )

  private val fraudCountries = Vector("RO", "NG", "BR", "VN", "ID", "UA", "PH")

  /** (mcc, card-not-present, share of the merchant pool). */
  private val categories: Vector[(Int, Boolean, Double)] = Vector(
    (5411, false, 0.22), // grocery
    (5812, false, 0.18), // restaurants
    (5541, false, 0.1), // fuel
    (5651, true, 0.1), // clothing
    (5732, true, 0.08), // electronics
    (5818, true, 0.12), // digital goods
    (4511, true, 0.05), // airlines
    (7995, true, 0.05), // gambling
    (4829, true, 0.05), // money transfer
    (6051, true, 0.05) // quasi-cash / crypto
  )

  private val riskyMccs = Set(5732, 7995, 4829, 6051)

  /**
   * Imperative wrapper so generation code reads top to bottom; all randomness flows from one seed.
   */
  final private class Rng(seed: Long):
    private var gen = SplitMix64(seed)
    def draw[A](r: Rand[A]): A =
      val (next, a) = r.run(gen)
      gen = next
      a
    def double(): Double = draw(Rand.double)
    def int(bound: Int): Int = draw(Rand.int(bound))
    def chance(p: Double): Boolean = draw(Rand.bernoulli(p))
    def pick[A](xs: IndexedSeq[A]): A = draw(Rand.oneOf(xs))

  def run(config: SimConfig): SimResult =
    val rng = Rng(config.seed)
    val periodMillis = config.days.toLong * 86_400_000L
    val startMillis = config.start.toEpochMilli
    var nextId = 0L
    def txId(): String =
      nextId += 1
      f"tx-$nextId%08d"

    def amount(usd: Double, currency: Currency): Money =
      val major = BigDecimal(usd / FxRates.reference.toUsd(currency))
        .setScale(currency.exponent, BigDecimal.RoundingMode.HALF_UP)
        .max(BigDecimal(1, currency.exponent))
      Money.fromMajor(major, currency).fold(e => throw IllegalStateException(e), identity)

    def randomNetwork(): String = s"${11 + rng.int(212)}.${rng.int(256)}.${rng.int(256)}"

    def ip(network: String): IpAddress =
      IpAddress.parse(s"$network.${1 + rng.int(254)}").fold(
        e => throw IllegalStateException(e),
        identity
      )

    /** Daytime-weighted timestamp: 85% of activity between 08:00 and 22:00 UTC-local. */
    def activityTime(): Long =
      val day = rng.int(config.days).toLong
      val hour = if rng.chance(0.85) then 8 + rng.int(14) else rng.int(24)
      startMillis + day * 86_400_000L + hour * 3_600_000L + rng.int(3_600_000)

    val merchants = Vector.tabulate(config.merchants) { i =>
      val (mcc, cnp, _) = rng.draw(Rand.weighted(categories.map(c => (c, c._3))))
      Merchant(f"m-$i%04d", mcc, cnp)
    }
    val everydayMerchants = merchants.filterNot(m => riskyMccs.contains(m.mcc))
    val riskyMerchants = merchants.filter(m => riskyMccs.contains(m.mcc))

    val cardholders = Vector.tabulate(config.cardholders) { i =>
      val (country, currency, _) = rng.draw(Rand.weighted(homeCountries.map(c => (c, c._3))))
      Cardholder(
        account = f"acct-$i%05d",
        cards = Vector.tabulate(if rng.chance(0.25) then 2 else 1)(k => f"card-$i%05d-$k"),
        country = CountryCode.unsafe(country),
        currency = currency,
        devices = Vector.tabulate(1 + rng.int(2))(k => f"dev-$i%05d-$k"),
        network = randomNetwork(),
        merchants = Vector.fill(4 + rng.int(6))(rng.pick(everydayMerchants)) :+ rng.pick(merchants),
        spendMu = math.log(15 + rng.double() * 60),
        dailyRate = 0.2 + rng.double() * 1.8,
        traveler = rng.chance(0.05),
        microSpender = rng.chance(0.04)
      )
    }

    val txs = Vector.newBuilder[Transaction]
    val labels = Vector.newBuilder[Label]

    def emit(
        holder: Cardholder,
        card: String,
        at: Long,
        merchant: Merchant,
        usd: Double,
        device: Option[String],
        ipAddr: Option[IpAddress],
        ipCountry: Option[CountryCode],
        fraud: Boolean
    ): Unit =
      val id = txId()
      val channel =
        if !merchant.cnp then Channel.Pos
        else
          rng.draw(Rand.weighted(Seq(
            Channel.Ecommerce -> 0.7,
            Channel.InApp -> 0.25,
            Channel.Moto -> 0.05
          )))
      val present = channel == Channel.Pos
      val tx = Transaction(
        id = id,
        timestamp = Instant.ofEpochMilli(at),
        accountId = holder.account,
        cardId = card,
        merchantId = merchant.id,
        mcc = merchant.mcc,
        amount = amount(usd, holder.currency),
        channel = channel,
        deviceId = if present then None else device,
        ip = if present then None else ipAddr,
        ipCountry = if present then None else ipCountry,
        billingCountry = holder.country
      )
      txs += tx
      if fraud && !rng.chance(config.unreportedFraudShare) then
        val delay = Duration.ofDays(3 + rng.int(25).toLong).plusHours(rng.int(24).toLong)
        labels += Label(id, card, Outcome.Fraud, tx.timestamp.plus(delay))

    // Legitimate activity.
    cardholders.foreach { h =>
      val count = rng.draw(Rand.poisson(h.dailyRate * config.days))
      (0 until count).foreach { _ =>
        val traveling = rng.chance(if h.traveler then 0.35 else config.travelProbability)
        val country =
          if traveling then
            CountryCode.unsafe(rng.pick(homeCountries.map(_._1).filterNot(_ == h.country.value)))
          else h.country
        val device =
          if rng.chance(if traveling then 0.2 else config.newDeviceProbability) then
            f"dev-new-${rng.int(1_000_000)}%06d"
          else rng.pick(h.devices)
        val small = rng.chance(0.04)
        val usd = if small then 0.5 + rng.double() * 4 else rng.draw(Rand.logNormal(h.spendMu, 0.6))
        val merchant = if rng.chance(0.9) then rng.pick(h.merchants) else rng.pick(merchants)
        val network = if traveling then randomNetwork() else h.network
        emit(
          h,
          rng.pick(h.cards),
          activityTime(),
          merchant,
          usd,
          Some(device),
          Some(ip(network)),
          Some(country),
          fraud = false
        )
      }
    }

    // Legitimate bursts of small charges (transit taps, vending, app top-ups) that look like card
    // testing to a naive rule.
    val transit = Merchant("m-transit", 4111, cnp = false)
    val topUp = Merchant("m-topup", 5818, cnp = true)
    cardholders.filter(_.microSpender).foreach { h =>
      (0 until 1 + rng.int(config.days / 3 + 1)).foreach { _ =>
        var t = activityTime()
        val merchant = if rng.chance(0.5) then transit else topUp
        (0 until 3 + rng.int(4)).foreach { _ =>
          t += 60_000L + rng.int(900_000)
          emit(
            h,
            h.cards.head,
            t,
            merchant,
            0.5 + rng.double() * 4,
            Some(h.devices.head),
            Some(ip(h.network)),
            Some(h.country),
            fraud = false
          )
        }
      }
    }

    def fraudLocation(victim: Cardholder): (String, CountryCode) =
      val country =
        if rng.chance(0.3) then victim.country else CountryCode.unsafe(rng.pick(fraudCountries))
      (randomNetwork(), country)

    // Card testing: burst of tiny charges, then cash-out.
    (0 until config.cardTestingAttacks).foreach { k =>
      val victim = rng.pick(cardholders)
      val card = rng.pick(victim.cards)
      val (net, country) = fraudLocation(victim)
      val device = f"fraud-ct-$k%04d"
      var t = activityTime()
      val probes = 4 + rng.int(9)
      (0 until probes).foreach { _ =>
        t += 20_000L + rng.int(150_000)
        val m = rng.pick(merchants.filter(_.cnp))
        emit(
          victim,
          card,
          t,
          m,
          0.5 + rng.double() * 2.5,
          Some(device),
          Some(ip(net)),
          Some(country),
          fraud = true
        )
      }
      (0 until 1 + rng.int(3)).foreach { _ =>
        t += 120_000L + rng.int(1_200_000)
        emit(
          victim,
          card,
          t,
          rng.pick(riskyMerchants),
          200 + rng.double() * 1300,
          Some(device),
          Some(ip(net)),
          Some(country),
          fraud = true
        )
      }
    }

    // Account takeover: new device, foreign IP, large risky purchases, often at night.
    (0 until config.takeovers).foreach { k =>
      val victim = rng.pick(cardholders)
      val card = rng.pick(victim.cards)
      val (net, country) = fraudLocation(victim)
      val device = f"fraud-ato-$k%04d"
      val nightly = rng.chance(0.6)
      var t =
        if nightly then
          startMillis + rng.int(config.days).toLong * 86_400_000L + rng.int(6).toLong * 3_600_000L
        else activityTime()
      (0 until 1 + rng.int(3)).foreach { _ =>
        t += 60_000L + rng.int(3_600_000)
        val m =
          if rng.chance(0.75) then rng.pick(riskyMerchants) else rng.pick(merchants.filter(_.cnp))
        emit(
          victim,
          card,
          t,
          m,
          300 + rng.double() * 2200,
          Some(device),
          Some(ip(net)),
          Some(country),
          fraud = true
        )
      }
    }

    // Rings: shared devices and one network cycling through many stolen cards.
    (0 until config.fraudRings).foreach { r =>
      val devices = Vector.tabulate(2 + rng.int(3))(d => f"fraud-ring-$r%02d-$d")
      val net = randomNetwork()
      val country = CountryCode.unsafe(rng.pick(fraudCountries))
      val victims = Vector.fill(5 + rng.int(11))(rng.pick(cardholders))
      victims.foreach { victim =>
        val card = rng.pick(victim.cards)
        val base = activityTime()
        (0 until 1 + rng.int(4)).foreach { j =>
          val at = base + j * (600_000L + rng.int(7_200_000))
          emit(
            victim,
            card,
            math.min(at, startMillis + periodMillis - 1),
            rng.pick(merchants.filter(_.cnp)),
            50 + rng.double() * 350,
            Some(rng.pick(devices)),
            Some(ip(net)),
            Some(country),
            fraud = true
          )
        }
      }
    }

    SimResult(txs.result().sortBy(t => (t.timestamp, t.id)), labels.result().sortBy(_.reportedAt))
