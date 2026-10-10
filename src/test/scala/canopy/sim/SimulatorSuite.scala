package canopy.sim

import canopy.domain.Outcome

class SimulatorSuite extends munit.FunSuite:
  private val small = SimConfig(
    seed = 11,
    cardholders = 150,
    days = 10,
    cardTestingAttacks = 5,
    takeovers = 5,
    fraudRings = 2
  )

  test("same seed gives an identical dataset; a different seed does not") {
    val a = Simulator.run(small)
    val b = Simulator.run(small)
    val c = Simulator.run(small.copy(seed = 12))
    assertEquals(a, b)
    assertNotEquals(a.transactions.map(_.amount), c.transactions.map(_.amount))
  }

  test("transactions are chronological with unique ids") {
    val r = Simulator.run(small)
    val ts = r.transactions.map(_.timestamp)
    assertEquals(ts, ts.sorted)
    assertEquals(r.transactions.map(_.id).distinct.size, r.transactions.size)
  }

  test("fraud is present, rare, and labelled after it happened") {
    val r = Simulator.run(small)
    val byId = r.transactions.map(t => t.id -> t).toMap
    val share = r.fraudIds.size.toDouble / r.transactions.size
    assert(share > 0.005 && share < 0.2, share)
    r.labels.foreach { l =>
      assertEquals(l.outcome, Outcome.Fraud)
      assert(l.reportedAt.isAfter(byId(l.transactionId).timestamp))
    }
  }

  test("card-present transactions carry no device or IP") {
    val r = Simulator.run(small)
    r.transactions.filterNot(_.channel.cardNotPresent).foreach { t =>
      assertEquals(t.deviceId, None)
      assertEquals(t.ip, None)
    }
  }

  test("random primitives are deterministic and in range") {
    val g = SplitMix64(42)
    val (_, d) = g.nextDouble
    assert(d >= 0 && d < 1)
    assertEquals(SplitMix64(42).nextLong, SplitMix64(42).nextLong)
    val ints = Rand.replicate(1000, Rand.int(7)).sample(1)
    assert(ints.forall(i => i >= 0 && i < 7))
    assertEquals(ints.toSet.size, 7)
    val normals = Rand.replicate(20_000, Rand.gaussian).sample(2)
    val mean = normals.sum / normals.size
    val variance = normals.map(x => (x - mean) * (x - mean)).sum / normals.size
    assertEqualsDouble(mean, 0.0, 0.03)
    assertEqualsDouble(variance, 1.0, 0.05)
    val pois = Rand.replicate(20_000, Rand.poisson(3.0)).sample(3)
    assertEqualsDouble(pois.sum.toDouble / pois.size, 3.0, 0.08)
  }

  test("weighted choice follows its weights") {
    val picks = Rand.replicate(10_000, Rand.weighted(Seq("a" -> 1.0, "b" -> 3.0))).sample(5)
    assertEqualsDouble(picks.count(_ == "b").toDouble / picks.size, 0.75, 0.02)
  }
