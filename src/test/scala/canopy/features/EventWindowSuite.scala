package canopy.features

class EventWindowSuite extends munit.FunSuite:
  private val Hour = 3_600_000L
  private val base = 1_000_000_000L

  private def build(events: (Long, Double, String)*): EventWindow =
    events.foldLeft(EventWindow.empty(24 * Hour))((w, e) => w.add(e._1, e._2, e._3))

  test("counts and sums over half-open windows") {
    val w = build((base, 10, "a"), (base + Hour, 20, "b"), (base + 2 * Hour, 30, "a"))
    val now = base + 2 * Hour
    assertEquals(w.count(now, Hour), 1)
    assertEquals(w.count(now, Hour + 1), 2)
    assertEquals(w.count(now, 3 * Hour), 3)
    assertEqualsDouble(w.sum(now, Hour + 1), 50.0, 1e-9)
    assertEquals(w.distinctKeys(now, 3 * Hour), 2)
  }

  test("queries ignore entries after 'now'") {
    val w = build((base, 1, "a"), (base + Hour, 2, "b"))
    assertEquals(w.count(base, Hour), 1)
    assertEqualsDouble(w.sum(base, Hour), 1.0, 1e-9)
  }

  test("old entries are evicted but sums stay exact") {
    val w = build((base, 5, "a"), (base + 10 * Hour, 7, "b"), (base + 25 * Hour, 11, "c"))
    assertEquals(w.size, 2)
    assertEquals(w.oldest, Some(base + 10 * Hour))
    assertEqualsDouble(w.sum(base + 25 * Hour, 24 * Hour), 18.0, 1e-9)
  }

  test("out-of-order events inside the horizon are spliced in and cumulative sums rebuilt") {
    val w = build((base, 1, "a"), (base + 2 * Hour, 4, "c"), (base + Hour, 2, "b"))
    assertEquals(w.entries.map(_.ts), Vector(base, base + Hour, base + 2 * Hour))
    assertEqualsDouble(w.sum(base + 2 * Hour, Hour + 1), 6.0, 1e-9)
    assertEqualsDouble(w.sum(base + Hour, Hour), 2.0, 1e-9)
  }

  test("events older than the horizon are dropped as too late") {
    val w = build((base + 30 * Hour, 1, "a"), (base, 99, "late"))
    assertEquals(w.size, 1)
  }

  test("lastBefore finds the previous event strictly before a timestamp") {
    val w = build((base, 1, "a"), (base + Hour, 1, "b"))
    assertEquals(w.lastBefore(base + Hour), Some(base))
    assertEquals(w.lastBefore(base), None)
  }

  test("countWhere filters by value") {
    val w = build((base, 1, "a"), (base + 1, 3, "a"), (base + 2, 50, "a"))
    assertEquals(w.countWhere(base + 2, Hour)(_ < 5), 2)
  }

  test("sum agrees with a brute-force reference on random streams") {
    val rnd = scala.util.Random(42)
    (1 to 25).foreach { _ =>
      val events = (1 to 200).map(_ => (base + rnd.nextLong(48 * Hour), rnd.nextDouble() * 100, "k"))
      val w = events.foldLeft(EventWindow.empty(6 * Hour))((acc, e) => acc.add(e._1, e._2, e._3))
      val retained = w.entries.map(e => (e.ts, e.value))
      val now = w.newest.get
      List(Hour, 3 * Hour, 6 * Hour).foreach { width =>
        val expected = retained.filter((ts, _) => ts > now - width && ts <= now).map(_._2).sum
        assertEqualsDouble(w.sum(now, width), expected, 1e-6)
      }
    }
  }
