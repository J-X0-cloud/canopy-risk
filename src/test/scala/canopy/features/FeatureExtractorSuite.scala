package canopy.features

import canopy.TestData.*
import canopy.domain.Channel

class FeatureExtractorSuite extends munit.FunSuite:
  private val extractor = FeatureExtractor()

  private def run(txs: List[canopy.domain.Transaction]): (FeatureState, List[FeatureVector]) =
    txs.foldLeft((FeatureState.empty, List.empty[FeatureVector])) { case ((s, acc), t) =>
      val (next, fv) = extractor.observe(s, t)
      (next, acc :+ fv)
    }

  test("first transaction on a card is marked new and has no history features") {
    val (_, List(fv)) = run(List(tx("t1"))): @unchecked
    assertEquals(fv.get("card.is_new"), Some(FeatureValue.Bool(true)))
    assertEquals(fv.num("card.count_1h"), Some(1.0))
    assertEquals(fv.get("card.amount_zscore"), None)
    assertEquals(fv.get("card.seconds_since_last"), None)
    assertEquals(fv.get("account.new_device"), Some(FeatureValue.Bool(true)))
  }

  test("velocity counts include the current event and respect window edges") {
    val txs = List(
      tx("t1", at = t0),
      tx("t2", at = t0.plusMinutes(5)),
      tx("t3", at = t0.plusMinutes(30)),
      tx("t4", at = t0.plusMinutes(61), amount = "2.00")
    )
    val (_, fvs) = run(txs)
    val last = fvs.last
    assertEquals(last.num("card.count_10m"), Some(1.0))
    assertEquals(last.num("card.count_1h"), Some(3.0))
    assertEquals(last.num("card.count_24h"), Some(4.0))
    assertEqualsDouble(last.num("card.sum_usd_1h").get, 25 + 25 + 2.0, 1e-9)
    assertEquals(last.num("card.small_count_1h"), Some(1.0))
    assertEquals(last.num("card.seconds_since_last"), Some(31 * 60.0))
    assertEquals(last.get("account.new_device"), Some(FeatureValue.Bool(false)))
  }

  test("devices and networks count distinct cards") {
    val txs = (1 to 5).toList.map(i => tx(s"t$i", card = s"card-$i", account = s"a-$i", at = t0.plusMinutes(i)))
    val (_, fvs) = run(txs)
    assertEquals(fvs.last.num("device.distinct_cards_24h"), Some(5.0))
    assertEquals(fvs.last.num("ip.distinct_cards_24h"), Some(5.0))
    assertEquals(fvs.last.num("ip.count_1h"), Some(5.0))
  }

  test("geo mismatch, country hopping and channel features") {
    val txs = List(
      tx("t1", ipCountry = Some("US")),
      tx("t2", ipCountry = Some("BR"), at = t0.plusMinutes(10)),
      tx("t3", ipCountry = Some("RO"), at = t0.plusMinutes(20), channel = Channel.Pos)
    )
    val (_, fvs) = run(txs)
    assertEquals(fvs.head.get("geo_mismatch"), Some(FeatureValue.Bool(false)))
    assertEquals(fvs(1).get("geo_mismatch"), Some(FeatureValue.Bool(true)))
    assertEquals(fvs.last.num("card.distinct_countries_24h"), Some(3.0))
    assertEquals(fvs.last.get("card_not_present"), Some(FeatureValue.Bool(false)))
  }

  test("amount z-score flags a large deviation from the card's habit") {
    val habit = (1 to 10).toList.map(i => tx(s"h$i", at = t0.plusHours(i.toLong), amount = s"${20 + i % 3}.00"))
    val spike = tx("spike", at = t0.plusHours(12), amount = "900.00")
    val (_, fvs) = run(habit :+ spike)
    assert(fvs.last.num("card.amount_zscore").get > 5.0, fvs.last.num("card.amount_zscore"))
  }

  test("transactions without device or IP omit those features entirely") {
    val (_, List(fv)) = run(List(tx("t1", device = None, ip = None, ipCountry = None))): @unchecked
    assertEquals(fv.get("device.count_1h"), None)
    assertEquals(fv.get("ip.count_1h"), None)
    assertEquals(fv.get("geo_mismatch"), None)
    assertEquals(fv.get("account.new_device"), Some(FeatureValue.Bool(false)))
  }

  test("an out-of-order event does not produce negative time since last") {
    val (_, fvs) = run(List(tx("t1", at = t0.plusMinutes(10)), tx("t0", at = t0)))
    assertEquals(fvs.last.get("card.seconds_since_last"), None)
  }

  test("idle entities are evicted") {
    val (state, _) = run(List(tx("t1"), tx("t2", card = "card-2", account = "a-2", device = Some("d-2"), at = t0.plusHours(24 * 50))))
    val swept = state.evictIdle(t0.plusHours(24 * 50).toEpochMilli, FeatureConfig().idleRetentionMillis)
    assertEquals(swept.cards.keySet, Set("card-2"))
    assertEquals(swept.devices.keySet, Set("d-2"))
  }

  test("every emitted feature is declared in the schema with a matching type") {
    val (_, fvs) = run(List(tx("t1"), tx("t2", at = t0.plusMinutes(1)), tx("t3", at = t0.plusMinutes(2)), tx("t4", at = t0.plusMinutes(3))))
    fvs.flatMap(_.values).foreach { (name, value) =>
      val declared = FeatureSchema.types.get(name)
      assert(declared.isDefined, s"$name missing from schema")
      val ok = (declared.get, value) match
        case (FeatureType.Number, FeatureValue.Num(_))   => true
        case (FeatureType.Boolean, FeatureValue.Bool(_)) => true
        case (FeatureType.Text, FeatureValue.Str(_))     => true
        case _                                           => false
      assert(ok, s"$name has value $value but is declared ${declared.get}")
    }
  }

  test("LRU device memory evicts the least recently used device") {
    val lru = (1 to 4).foldLeft(LruSet.empty(3))((s, i) => s.touch(s"d$i", i.toLong))
    assertEquals(lru.entries.keySet, Set("d2", "d3", "d4"))
    val touched = lru.touch("d2", 10).touch("d5", 11)
    assertEquals(touched.entries.keySet, Set("d2", "d4", "d5"))
  }
