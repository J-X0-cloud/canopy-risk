package canopy.sim

/**
 * SplitMix64 pseudo-random generator as a pure value. Deterministic across JVMs and Scala
 * versions, so a seed fully determines a simulated dataset and the model trained on it.
 */
final case class SplitMix64(state: Long):
  def nextLong: (SplitMix64, Long) =
    val s = state + 0x9e3779b97f4a7c15L
    var z = s
    z = (z ^ (z >>> 30)) * 0xbf58476d1ce4e5b9L
    z = (z ^ (z >>> 27)) * 0x94d049bb133111ebL
    (SplitMix64(s), z ^ (z >>> 31))

  /** Uniform in [0, 1) with 53 bits of precision. */
  def nextDouble: (SplitMix64, Double) =
    val (g, l) = nextLong
    (g, (l >>> 11).toDouble / (1L << 53).toDouble)

  /** Uniform integer in [0, bound) without modulo bias. */
  def nextInt(bound: Int): (SplitMix64, Int) =
    require(bound > 0, "bound must be positive")
    val limit = Long.MaxValue - (Long.MaxValue % bound)
    @scala.annotation.tailrec
    def go(g: SplitMix64): (SplitMix64, Int) =
      val (next, l) = g.nextLong
      val v = l >>> 1
      if v >= limit then go(next) else (next, (v % bound).toInt)
    go(this)

  /** An independent stream derived from this one, for per-entity generators. */
  def split: (SplitMix64, SplitMix64) =
    val (g1, a) = nextLong
    val (g2, _) = g1.nextLong
    (g2, SplitMix64(a))

/**
 * State monad style helpers so generators read as sequential code without threading the RNG by
 * hand. `Rand[A]` is simply `SplitMix64 => (SplitMix64, A)`.
 */
final case class Rand[A](run: SplitMix64 => (SplitMix64, A)):
  def map[B](f: A => B): Rand[B] = Rand { g =>
    val (g2, a) = run(g)
    (g2, f(a))
  }

  def flatMap[B](f: A => Rand[B]): Rand[B] = Rand { g =>
    val (g2, a) = run(g)
    f(a).run(g2)
  }

  def sample(seed: Long): A = run(SplitMix64(seed))._2

object Rand:
  def pure[A](a: A): Rand[A] = Rand(g => (g, a))

  val double: Rand[Double] = Rand(_.nextDouble)

  def int(bound: Int): Rand[Int] = Rand(_.nextInt(bound))

  def between(lo: Double, hi: Double): Rand[Double] = double.map(u => lo + u * (hi - lo))

  def bernoulli(p: Double): Rand[Boolean] = double.map(_ < p)

  def oneOf[A](xs: IndexedSeq[A]): Rand[A] =
    require(xs.nonEmpty, "cannot choose from an empty sequence")
    int(xs.size).map(xs)

  /** Weighted choice. Weights need not sum to one. */
  def weighted[A](choices: Seq[(A, Double)]): Rand[A] =
    val total = choices.map(_._2).sum
    double.map { u =>
      val target = u * total
      choices
        .scanLeft((Option.empty[A], 0.0)) { case ((_, acc), (a, w)) => (Some(a), acc + w) }
        .collectFirst { case (Some(a), cum) if cum > target => a }
        .getOrElse(choices.last._1)
    }

  /** Standard normal via Box-Muller. */
  val gaussian: Rand[Double] =
    for
      u1 <- double
      u2 <- double
    yield math.sqrt(-2.0 * math.log(1.0 - u1)) * math.cos(2 * math.Pi * u2)

  def logNormal(mu: Double, sigma: Double): Rand[Double] = gaussian.map(z => math.exp(mu + sigma * z))

  /** Exponential inter-arrival time with the given mean. */
  def exponential(mean: Double): Rand[Double] = double.map(u => -mean * math.log(1.0 - u))

  /** Poisson count via Knuth's method; adequate for the small means used in simulation. */
  def poisson(mean: Double): Rand[Int] =
    val limit = math.exp(-mean)
    def go(k: Int, p: Double): Rand[Int] =
      double.flatMap { u =>
        val next = p * u
        if next <= limit then pure(k) else go(k + 1, next)
      }
    go(0, 1.0)

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] = Rand { g =>
    val (finalG, acc) = rs.foldLeft((g, List.empty[A])) { case ((gen, out), r) =>
      val (g2, a) = r.run(gen)
      (g2, a :: out)
    }
    (finalG, acc.reverse)
  }

  def replicate[A](n: Int, r: Rand[A]): Rand[List[A]] = sequence(List.fill(n)(r))
