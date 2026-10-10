package canopy.ml

/** Hyper-parameters for [[LogisticRegression.fit]]. */
final case class TrainConfig(
    l2: Double = 1e-3,
    maxIterations: Int = 50,
    tolerance: Double = 1e-7,
    positiveWeight: Option[Double] = None
)

final case class FitResult(
    intercept: Double,
    coefficients: Array[Double],
    iterations: Int,
    converged: Boolean,
    finalLoss: Double
)

/**
 * L2-regularised logistic regression fitted by Newton-Raphson (iteratively reweighted least
 * squares) with a backtracking line search.
 *
 * Newton's method converges in a handful of iterations for the tens of features used here, and the
 * line search keeps it monotone when the starting point is far from the optimum or the data are
 * nearly separable. The intercept is not penalised.
 *
 * Fraud is rare, so positives may be up-weighted to give the optimiser a usable gradient. Weighting
 * inflates predicted probabilities by the weight ratio; [[fit]] removes that bias by shifting the
 * intercept by -ln(w), which is the standard prior correction for case-control sampling.
 */
object LogisticRegression:
  /** Numerically stable logistic function. */
  def sigmoid(z: Double): Double =
    if z >= 0 then 1.0 / (1.0 + math.exp(-z))
    else
      val e = math.exp(z)
      e / (1.0 + e)

  /** ln(1 + e^z) without overflow. */
  def softplus(z: Double): Double = if z > 0 then z + math.log1p(math.exp(-z)) else math.log1p(math.exp(z))

  def logit(p: Double): Double =
    val q = math.min(1 - 1e-15, math.max(1e-15, p))
    math.log(q / (1 - q))

  def fit(x: Array[Array[Double]], y: Array[Int], config: TrainConfig = TrainConfig()): FitResult =
    require(x.length == y.length, "rows and labels differ in length")
    require(x.nonEmpty, "cannot fit on an empty dataset")
    require(y.forall(v => v == 0 || v == 1), "labels must be 0 or 1")
    val n = x.length
    val d = x.head.length + 1
    val positives = y.count(_ == 1)
    val posWeight = config.positiveWeight.getOrElse(
      if positives == 0 || positives == n then 1.0 else (n - positives).toDouble / positives
    )
    val w = y.map(v => if v == 1 then posWeight else 1.0)
    val totalWeight = w.sum

    def design(i: Int, j: Int): Double = if j == 0 then 1.0 else x(i)(j - 1)

    def margins(beta: Array[Double]): Array[Double] =
      Array.tabulate(n) { i =>
        var z = beta(0)
        val row = x(i)
        var j = 0
        while j < row.length do
          z += beta(j + 1) * row(j)
          j += 1
        z
      }

    def loss(beta: Array[Double]): Double =
      val z = margins(beta)
      var s = 0.0
      var i = 0
      while i < n do
        // -[y log p + (1-y) log(1-p)] = softplus(z) - y z
        s += w(i) * (softplus(z(i)) - y(i) * z(i))
        i += 1
      var penalty = 0.0
      var j = 1
      while j < d do
        penalty += beta(j) * beta(j)
        j += 1
      s / totalWeight + 0.5 * config.l2 * penalty

    def gradientAndHessian(beta: Array[Double]): (Array[Double], Array[Array[Double]]) =
      val z = margins(beta)
      val g = Array.ofDim[Double](d)
      val h = Array.ofDim[Double](d, d)
      var i = 0
      while i < n do
        val p = sigmoid(z(i))
        val r = w(i) * (p - y(i)) / totalWeight
        val c = w(i) * p * (1 - p) / totalWeight
        var a = 0
        while a < d do
          val xa = design(i, a)
          g(a) += r * xa
          val cxa = c * xa
          var b = 0
          while b <= a do
            h(a)(b) += cxa * design(i, b)
            b += 1
          a += 1
        i += 1
      for a <- 0 until d; b <- 0 until a do h(b)(a) = h(a)(b)
      for j <- 1 until d do
        g(j) += config.l2 * beta(j)
        h(j)(j) += config.l2
      (g, h)

    // Start from the weighted base rate so the first Newton step is already well scaled.
    val beta0 = Array.ofDim[Double](d)
    val weightedPositives = (0 until n).map(i => if y(i) == 1 then w(i) else 0.0).sum
    beta0(0) = logit(weightedPositives / totalWeight)

    @scala.annotation.tailrec
    def iterate(beta: Array[Double], currentLoss: Double, it: Int): FitResult =
      val (g, h) = gradientAndHessian(beta)
      if Linalg.maxAbs(g) < config.tolerance then FitResult(beta(0), beta.tail, it, true, currentLoss)
      else if it >= config.maxIterations then FitResult(beta(0), beta.tail, it, false, currentLoss)
      else
        val step = Linalg.solveSpd(h, g)
        val slope = Linalg.dot(g, step)
        // Armijo backtracking: halve the step until the loss decreases sufficiently.
        @scala.annotation.tailrec
        def search(t: Double): Option[(Array[Double], Double)] =
          if t < 1e-10 then None
          else
            val candidate = Array.tabulate(d)(j => beta(j) - t * step(j))
            val l = loss(candidate)
            if l <= currentLoss - 1e-4 * t * slope then Some((candidate, l)) else search(t / 2)
        search(1.0) match
          case Some((next, nextLoss)) => iterate(next, nextLoss, it + 1)
          case None                   => FitResult(beta(0), beta.tail, it, Linalg.maxAbs(g) < 1e-4, currentLoss)

    val raw = iterate(beta0, loss(beta0), 0)
    raw.copy(intercept = raw.intercept - math.log(posWeight))
