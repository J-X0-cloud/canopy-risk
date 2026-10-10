package canopy.ml

/**
 * The small amount of dense linear algebra the trainer needs. Matrices are row-major
 * `Array[Array[Double]]`; dimensions are the number of model features (tens), so clarity wins over
 * blocking or SIMD.
 */
object Linalg:
  def dot(a: Array[Double], b: Array[Double]): Double =
    require(a.length == b.length, s"dimension mismatch ${a.length} vs ${b.length}")
    var s = 0.0
    var i = 0
    while i < a.length do
      s += a(i) * b(i)
      i += 1
    s

  def norm(a: Array[Double]): Double = math.sqrt(dot(a, a))

  def maxAbs(a: Array[Double]): Double = a.foldLeft(0.0)((m, x) => math.max(m, math.abs(x)))

  /**
   * Cholesky factorisation A = L Lᵀ of a symmetric positive-definite matrix. Returns None if a
   * non-positive pivot shows the matrix is not positive definite (to working precision).
   */
  def cholesky(a: Array[Array[Double]]): Option[Array[Array[Double]]] =
    val n = a.length
    val l = Array.ofDim[Double](n, n)
    var ok = true
    var j = 0
    while ok && j < n do
      var d = a(j)(j)
      var k = 0
      while k < j do
        d -= l(j)(k) * l(j)(k)
        k += 1
      if d <= 0 || d.isNaN then ok = false
      else
        l(j)(j) = math.sqrt(d)
        var i = j + 1
        while i < n do
          var s = a(i)(j)
          var k2 = 0
          while k2 < j do
            s -= l(i)(k2) * l(j)(k2)
            k2 += 1
          l(i)(j) = s / l(j)(j)
          i += 1
      j += 1
    Option.when(ok)(l)

  /** Solves L Lᵀ x = b given the Cholesky factor L. */
  def choleskySolve(l: Array[Array[Double]], b: Array[Double]): Array[Double] =
    val n = b.length
    val y = Array.ofDim[Double](n)
    for i <- 0 until n do
      var s = b(i)
      for k <- 0 until i do s -= l(i)(k) * y(k)
      y(i) = s / l(i)(i)
    val x = Array.ofDim[Double](n)
    for i <- (n - 1) to 0 by -1 do
      var s = y(i)
      for k <- (i + 1) until n do s -= l(k)(i) * x(k)
      x(i) = s / l(i)(i)
    x

  /**
   * Solves A x = b for symmetric positive (semi-)definite A. If the factorisation fails, a growing
   * diagonal jitter is added, which is the usual remedy for near-singular Hessians caused by
   * collinear or constant features.
   */
  def solveSpd(a: Array[Array[Double]], b: Array[Double]): Array[Double] =
    val n = a.length
    val scale = math.max(1e-12, (0 until n).map(i => math.abs(a(i)(i))).maxOption.getOrElse(1.0))
    def attempt(jitter: Double): Array[Double] =
      val m = Array.tabulate(n, n)((i, j) => if i == j then a(i)(j) + jitter else a(i)(j))
      cholesky(m) match
        case Some(l)                => choleskySolve(l, b)
        case None if jitter < scale => attempt(if jitter == 0 then scale * 1e-10 else jitter * 10)
        case None => throw ArithmeticException("matrix is not positive definite even with jitter")
    attempt(0.0)
