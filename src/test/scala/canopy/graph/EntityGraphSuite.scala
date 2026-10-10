package canopy.graph

import canopy.TestData.*
import canopy.domain.Outcome

class EntityGraphSuite extends munit.FunSuite:
  private val now = t0.toEpochMilli

  test("cards sharing a device are linked; sharing only an account is not") {
    val g = EntityGraph.empty()
      .record(tx("1", card = "c1", account = "a1", device = Some("d1"), ip = None))
      .record(tx("2", card = "c2", account = "a2", device = Some("d1"), ip = None))
      .record(tx("3", card = "c3", account = "a1", device = Some("d9"), ip = None))
    val f = g.featuresFor("c1", now)
    assertEquals(f.linkedCards, 1)
    assert(f.componentSize >= 5)
  }

  test("fraud labels on linked cards raise the ratio and can be cleared") {
    val g = (1 to 4)
      .foldLeft(EntityGraph.empty())((acc, i) =>
        acc.record(tx(s"$i", card = s"c$i", account = s"a$i", device = Some("shared"), ip = None))
      )
      .label("c2", Outcome.Fraud, now)
      .label("c3", Outcome.Fraud, now)
    val f = g.featuresFor("c1", now)
    assertEquals(f.linkedCards, 3)
    assertEquals(f.fraudCards, 2)
    assertEqualsDouble(f.fraudRatio, 2.0 / 3, 1e-9)
    assertEquals(g.label("c2", Outcome.Legit, now).featuresFor("c1", now).fraudCards, 1)
  }

  test("expired edges are invisible and pruned") {
    val g = EntityGraph.empty(GraphConfig(edgeTtlMillis = 3_600_000L))
      .record(tx("1", card = "c1", device = Some("d"), ip = None))
      .record(tx("2", card = "c2", account = "a2", device = Some("d"), ip = None))
    val later = now + 2 * 3_600_000L
    assertEquals(g.featuresFor("c1", later).linkedCards, 0)
    assertEquals(g.prune(later).nodeCount, 0)
  }

  test("hub nodes are reached but not expanded") {
    val config = GraphConfig(hubDegree = 3)
    val g = (1 to 10).foldLeft(EntityGraph.empty(config))((acc, i) =>
      acc.record(tx(
        s"$i",
        card = s"c$i",
        account = s"a$i",
        device = None,
        ip = Some("198.51.100.1")
      ))
    )
    val f = g.featuresFor("c1", now)
    assertEquals(f.linkedCards, 0)
  }

  test("node cap truncates the search and reports it") {
    val g = (1 to 50).foldLeft(EntityGraph.empty(GraphConfig(maxNodes = 10, hubDegree = 1000)))(
      (acc, i) =>
        acc.record(tx(s"$i", card = s"c$i", account = s"a$i", device = Some("d"), ip = None))
    )
    val f = g.featuresFor("c1", now)
    assert(f.truncated)
    assertEquals(f.componentSize, 10)
  }

  test("depth limit stops long chains") {
    // c0 - d0 - c1 - d1 - c2 - d2 - c3 : card c3 is 6 hops from c0
    val g = (0 to 2).foldLeft(EntityGraph.empty(GraphConfig(maxDepth = 4)))((acc, i) =>
      acc
        .link(Node.Card(s"c$i"), Node.Device(s"d$i"), now)
        .link(Node.Device(s"d$i"), Node.Card(s"c${i + 1}"), now)
    )
    assertEquals(g.featuresFor("c0", now).linkedCards, 2)
  }

  test("components groups linked entities, largest first") {
    val g = EntityGraph.empty()
      .link(Node.Card("a"), Node.Device("x"), now)
      .link(Node.Card("b"), Node.Device("x"), now)
      .link(Node.Card("c"), Node.Device("y"), now)
    val comps = g.components(now)
    assertEquals(comps.map(_.size), List(3, 2))
    assertEquals(g.edgeCount, 3)
  }

  test("self links are ignored") {
    assertEquals(EntityGraph.empty().link(Node.Card("a"), Node.Card("a"), now).nodeCount, 0)
  }

  test("union-find merges and counts sets") {
    val uf = UnionFind(5)
    assert(uf.union(0, 1))
    assert(uf.union(1, 2))
    assert(!uf.union(0, 2))
    assert(uf.connected(0, 2))
    assert(!uf.connected(0, 3))
    assertEquals(uf.count, 3)
  }
