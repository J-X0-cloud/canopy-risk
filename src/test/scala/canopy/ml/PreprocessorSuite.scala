package canopy.ml

import canopy.features.FeatureVector

class PreprocessorSuite extends munit.FunSuite:
  private def fv(pairs: (String, Double)*): FeatureVector =
    pairs.foldLeft(FeatureVector.empty)((v, p) => v.withNum(p._1, p._2))

  test("skewed non-negative features get a log transform, symmetric ones do not") {
    val rows = (1 to 200).map(i => fv("count" -> (if i % 20 == 0 then 500.0 else 1.0), "sym" -> (i % 7 - 3.0)))
    val pre = Preprocessor.fit(Vector("count", "sym"), rows)
    assertEquals(pre.specs.map(_.transform), Vector(Transform.Log1p, Transform.Identity))
  }

  test("missing indicators are added only for features that were sometimes absent") {
    val rows = Vector(fv("a" -> 1, "b" -> 2), fv("a" -> 3))
    val pre = Preprocessor.fit(Vector("a", "b"), rows)
    assertEquals(pre.columnNames, Vector("a", "b", "b:missing"))
    assertEquals(pre.row(fv("a" -> 2)).toVector, Vector(0.0, 0.0, 1.0))
  }

  test("constant features standardise to zero rather than dividing by zero") {
    val pre = Preprocessor.fit(Vector("c"), Vector.fill(10)(fv("c" -> 4)))
    assertEquals(pre.specs.head.std, 1.0)
    assertEquals(pre.row(fv("c" -> 4)).toVector, Vector(0.0))
  }

  test("extreme values are clipped") {
    val rows = (1 to 100).map(i => fv("x" -> (i % 2).toDouble))
    val pre = Preprocessor.fit(Vector("x"), rows, clip = 3.0)
    assertEquals(pre.row(fv("x" -> 1e9)).toVector, Vector(3.0))
  }

  test("skewness of a symmetric sample is zero") {
    assertEqualsDouble(Preprocessor.skewness(Array(1.0, 2.0, 3.0, 4.0, 5.0)), 0.0, 1e-12)
  }
