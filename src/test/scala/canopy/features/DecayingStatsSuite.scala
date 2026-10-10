package canopy.features

class DecayingStatsSuite extends munit.FunSuite:
  private val Day = 86_400_000L

  test("without decay between updates it matches the plain mean and population variance") {
    val xs = List(2.0, 4.0, 4.0, 4.0, 5.0, 5.0, 7.0, 9.0)
    val s = xs.foldLeft(DecayingStats.empty(Day))((acc, x) => acc.update(0L, x))
    assertEqualsDouble(s.mean, 5.0, 1e-12)
    assertEqualsDouble(s.variance, 4.0, 1e-12)
    assertEqualsDouble(s.stdDev, 2.0, 1e-12)
  }

  test("older observations lose weight with the half-life") {
    val s = DecayingStats.empty(Day).update(0L, 0.0).update(Day, 10.0)
    assertEqualsDouble(s.weight, 1.5, 1e-12)
    assertEqualsDouble(s.mean, 10.0 * (1.0 / 1.5), 1e-12)
  }

  test("z-score needs a minimum history and respects the variance floor") {
    val s = List(1.0, 1.0).foldLeft(DecayingStats.empty(Day))((a, x) => a.update(0L, x))
    assertEquals(s.zScore(5.0), None)
    val s3 = s.update(0L, 1.0)
    assertEqualsDouble(s3.zScore(2.0).get, 1.0 / math.sqrt(0.05), 1e-9)
  }

  test("updates arriving out of order do not inflate weights") {
    val s = DecayingStats.empty(Day).update(Day, 1.0).update(0L, 3.0)
    assertEqualsDouble(s.weight, 2.0, 1e-12)
    assertEquals(s.lastTs, Day)
  }
