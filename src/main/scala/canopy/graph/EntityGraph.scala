package canopy.graph

import canopy.domain.{Outcome, Transaction}

import scala.annotation.tailrec
import scala.collection.immutable.Queue

/**
 * A vertex in the entity graph. Cards are linked to the devices, networks and accounts they use.
 */
enum Node:
  case Card(id: String)
  case Device(id: String)
  case Network(cidr: String)
  case Account(id: String)

  def key: String = this match
    case Card(id)      => s"card:$id"
    case Device(id)    => s"device:$id"
    case Network(cidr) => s"net:$cidr"
    case Account(id)   => s"account:$id"

/** Limits that keep neighbourhood queries cheap and robust against hub nodes. */
final case class GraphConfig(
    edgeTtlMillis: Long = 30L * 24 * 3600 * 1000,
    maxDepth: Int = 4,
    maxNodes: Int = 500,
    hubDegree: Int = 200
)

/** Graph-derived features for one card at one moment. */
final case class GraphFeatures(
    linkedCards: Int,
    componentSize: Int,
    fraudCards: Int,
    truncated: Boolean
):
  def fraudRatio: Double = if linkedCards == 0 then 0.0 else fraudCards.toDouble / linkedCards

/**
 * An undirected multi-entity graph with time-stamped edges.
 *
 * Each edge remembers when it was last observed; edges older than the TTL are invisible to queries
 * and removed by [[prune]]. Confirmed-fraud labels attach to card nodes so that a new card which
 * shares a phone or a /24 with previously charged-back cards inherits that risk.
 *
 * Hub nodes (a carrier-grade NAT, a shared kiosk) would otherwise connect every customer to every
 * other. Nodes whose live degree exceeds `hubDegree` are counted when reached but never expanded.
 */
final case class EntityGraph(
    config: GraphConfig,
    adjacency: Map[Node, Map[Node, Long]],
    fraudCards: Map[String, Long]
):
  def nodeCount: Int = adjacency.size

  def edgeCount: Int = adjacency.valuesIterator.map(_.size).sum / 2

  def link(a: Node, b: Node, ts: Long): EntityGraph =
    if a == b then this
    else
      def touch(adj: Map[Node, Map[Node, Long]], from: Node, to: Node) =
        val edges = adj.getOrElse(from, Map.empty)
        adj.updated(from, edges.updated(to, math.max(ts, edges.getOrElse(to, Long.MinValue))))
      copy(adjacency = touch(touch(adjacency, a, b), b, a))

  /** Records the relationships implied by one transaction. */
  def record(tx: Transaction): EntityGraph =
    val ts = tx.timestamp.toEpochMilli
    val card = Node.Card(tx.cardId)
    val withAccount = link(card, Node.Account(tx.accountId), ts)
    val withDevice = tx.deviceId.fold(withAccount)(d => withAccount.link(card, Node.Device(d), ts))
    tx.ip.fold(withDevice)(ip => withDevice.link(card, Node.Network(ip.network), ts))

  def label(cardId: String, outcome: Outcome, ts: Long): EntityGraph = outcome match
    case Outcome.Fraud => copy(fraudCards = fraudCards.updated(cardId, ts))
    case Outcome.Legit => copy(fraudCards = fraudCards - cardId)

  def isFraud(cardId: String): Boolean = fraudCards.contains(cardId)

  def neighbours(node: Node, now: Long): Iterable[Node] =
    val cutoff = now - config.edgeTtlMillis
    adjacency.getOrElse(node, Map.empty).collect { case (n, seen) if seen >= cutoff => n }

  def degree(node: Node, now: Long): Int = neighbours(node, now).size

  /**
   * Bounded breadth-first exploration from a card. Accounts are traversed but do not by themselves
   * make two cards "linked": a household sharing an account is normal, sharing a device fingerprint
   * across unrelated accounts is not. Linked cards are those reached through at least one device or
   * network hop.
   */
  def featuresFor(cardId: String, now: Long): GraphFeatures =
    val start = Node.Card(cardId)

    final case class Visit(node: Node, depth: Int, viaShared: Boolean)

    @tailrec
    def bfs(
        queue: Queue[Visit],
        seen: Set[Node],
        linked: Set[String],
        truncated: Boolean
    ): (Set[Node], Set[String], Boolean) =
      queue.dequeueOption match
        case None => (seen, linked, truncated)
        case Some((Visit(node, depth, viaShared), rest)) =>
          val isHub = node != start && degree(node, now) > config.hubDegree
          if depth >= config.maxDepth || isHub then bfs(rest, seen, linked, truncated)
          else
            val candidates = neighbours(node, now).filterNot(seen.contains).toVector
            val room = config.maxNodes - seen.size
            val admitted = candidates.take(math.max(0, room))
            val nowTruncated = truncated || admitted.size < candidates.size
            val nextVisits = admitted.map { n =>
              val shared = viaShared || n.isInstanceOf[Node.Device] || n.isInstanceOf[Node.Network]
              Visit(n, depth + 1, shared)
            }
            val newLinked = nextVisits.collect {
              case Visit(Node.Card(id), _, true) if id != cardId => id
            }
            bfs(rest.enqueueAll(nextVisits), seen ++ admitted, linked ++ newLinked, nowTruncated)

    val (seen, linked, truncated) = bfs(Queue(Visit(start, 0, false)), Set(start), Set.empty, false)
    GraphFeatures(
      linkedCards = linked.size,
      componentSize = seen.size,
      fraudCards = linked.count(fraudCards.contains),
      truncated = truncated
    )

  /** Drops expired edges and any node left without edges. */
  def prune(now: Long): EntityGraph =
    val cutoff = now - config.edgeTtlMillis
    val live = adjacency.view
      .mapValues(_.filter(_._2 >= cutoff))
      .filter(_._2.nonEmpty)
      .toMap
    copy(adjacency = live)

  /** Connected components over live edges, via union-find. Used for offline ring analysis. */
  def components(now: Long): List[Set[Node]] =
    val nodes = adjacency.keys.toVector
    val uf = UnionFind(nodes.size)
    val index = nodes.zipWithIndex.toMap
    for
      n <- nodes
      m <- neighbours(n, now)
      j <- index.get(m)
    do uf.union(index(n), j)
    nodes.indices
      .groupBy(uf.find)
      .values
      .map(_.map(nodes).toSet)
      .filter(_.size > 1)
      .toList
      .sortBy(c => -c.size)

object EntityGraph:
  def empty(config: GraphConfig = GraphConfig()): EntityGraph =
    EntityGraph(config, Map.empty, Map.empty)
