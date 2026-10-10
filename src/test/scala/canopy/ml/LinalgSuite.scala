package canopy.ml

class LinalgSuite extends munit.FunSuite:
  test("cholesky reconstructs a known SPD matrix") {
    val a = Array(Array(4.0, 12.0, -16.0), Array(12.0, 37.0, -43.0), Array(-16.0, -43.0, 98.0))
    val l = Linalg.cholesky(a).get
    assertEquals(l.map(_.toVector).toVector, Vector(Vector(2.0, 0.0, 0.0), Vector(6.0, 1.0, 0.0), Vector(-8.0, 5.0, 3.0)))
  }

  test("cholesky rejects an indefinite matrix") {
    assertEquals(Linalg.cholesky(Array(Array(1.0, 2.0), Array(2.0, 1.0))), None)
  }

  test("solveSpd solves a linear system") {
    val a = Array(Array(4.0, 1.0), Array(1.0, 3.0))
    val x = Linalg.solveSpd(a, Array(1.0, 2.0))
    assertEqualsDouble(x(0), 1.0 / 11, 1e-12)
    assertEqualsDouble(x(1), 7.0 / 11, 1e-12)
  }

  test("solveSpd survives a singular matrix by adding jitter") {
    val a = Array(Array(1.0, 1.0), Array(1.0, 1.0))
    val x = Linalg.solveSpd(a, Array(2.0, 2.0))
    assertEqualsDouble(x(0) + x(1), 2.0, 1e-4)
  }

  test("dot rejects mismatched dimensions") {
    intercept[IllegalArgumentException](Linalg.dot(Array(1.0), Array(1.0, 2.0)))
  }
