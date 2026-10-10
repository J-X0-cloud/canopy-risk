package canopy.graph

/**
 * Disjoint-set forest with union by rank and path halving. Mutable by design: it is created, filled
 * and read inside a single method and never escapes.
 */
final class UnionFind(size: Int):
  private val parent = Array.tabulate(size)(identity)
  private val rank = Array.fill(size)(0)
  private var sets = size

  def count: Int = sets

  def find(x: Int): Int =
    var i = x
    while parent(i) != i do
      parent(i) = parent(parent(i))
      i = parent(i)
    i

  /** Merges the sets containing `a` and `b`; returns false if they were already joined. */
  def union(a: Int, b: Int): Boolean =
    val ra = find(a)
    val rb = find(b)
    if ra == rb then false
    else
      if rank(ra) < rank(rb) then parent(ra) = rb
      else if rank(ra) > rank(rb) then parent(rb) = ra
      else
        parent(rb) = ra
        rank(ra) += 1
      sets -= 1
      true

  def connected(a: Int, b: Int): Boolean = find(a) == find(b)
