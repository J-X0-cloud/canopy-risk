package canopy.rules

import canopy.features.{FeatureType, FeatureValue, FeatureVector}

class EvaluatorSuite extends munit.FunSuite:
  private val schema = Map(
    "a" -> FeatureType.Number,
    "b" -> FeatureType.Number,
    "flag" -> FeatureType.Boolean,
    "s" -> FeatureType.Text
  )
  private val typer = Typer.forSchema(schema)

  private def eval(src: String, fv: FeatureVector): Value =
    val parsed = Parser.parseExpr(src).fold(d => fail(d.render(src)), identity)
    val typed = typer.inferExpr(parsed).fold(ds => fail(ds.map(_.summary).mkString), identity)
    Evaluator.compile(typed)(fv)

  private val full = FeatureVector(
    Map(
      "a" -> FeatureValue.Num(10),
      "b" -> FeatureValue.Num(4),
      "flag" -> FeatureValue.Bool(true),
      "s" -> FeatureValue.Str("Hello")
    )
  )
  private val empty = FeatureVector.empty

  import Value.*

  test("arithmetic and precedence") {
    assertEquals(eval("a + b * 2", full), VNum(18))
    assertEquals(eval("(a + b) * 2", full), VNum(28))
    assertEquals(eval("a % b", full), VNum(2))
    assertEquals(eval("-a + 1", full), VNum(-9))
  }

  test("division by zero is missing, not infinity") {
    assertEquals(eval("a / 0", full), Missing)
    assertEquals(eval("a % 0", full), Missing)
  }

  test("missing propagates through arithmetic and comparisons") {
    assertEquals(eval("a + 1", empty), Missing)
    assertEquals(eval("a > 1", empty), Missing)
    assertEquals(eval("s == \"x\"", empty), Missing)
  }

  test("Kleene and/or truth tables") {
    assertEquals(eval("a > 1 and false", empty), VBool(false))
    assertEquals(eval("false and a > 1", empty), VBool(false))
    assertEquals(eval("a > 1 and true", empty), Missing)
    assertEquals(eval("a > 1 or true", empty), VBool(true))
    assertEquals(eval("true or a > 1", empty), VBool(true))
    assertEquals(eval("a > 1 or false", empty), Missing)
    assertEquals(eval("not (a > 1)", empty), Missing)
  }

  test("membership with lists and SQL-style unknowns") {
    assertEquals(eval("a in [1, 10]", full), VBool(true))
    assertEquals(eval("a not in [1, 2]", full), VBool(true))
    assertEquals(eval("a in [1, b]", full), VBool(false))
    assertEquals(eval("a in [1, a + 1]", full), VBool(false))
    assertEquals(eval("5 in [1, a]", empty), Missing)
    assertEquals(eval("1 in [1, a]", empty), VBool(true))
    assertEquals(eval("5 not in [1, a]", empty), Missing)
  }

  test("substring membership") {
    assertEquals(eval("\"ell\" in s", full), VBool(true))
    assertEquals(eval("\"xyz\" not in s", full), VBool(true))
  }

  test("between is inclusive") {
    assertEquals(eval("a between 10 and 20", full), VBool(true))
    assertEquals(eval("a between 0 and 9.99", full), VBool(false))
    assertEquals(eval("a between b and 10", full), VBool(true))
    assertEquals(eval("a between 1 and 2", empty), Missing)
  }

  test("builtins") {
    assertEquals(eval("abs(-a)", full), VNum(10))
    assertEquals(eval("min(a, b, 7)", full), VNum(4))
    assertEquals(eval("max(a, b)", full), VNum(10))
    assertEquals(eval("max(a, 3)", empty), Missing)
    assertEquals(eval("log1p(0)", full), VNum(0))
    assertEquals(eval("log1p(-2)", full), Missing)
    assertEquals(eval("sqrt(-1)", full), Missing)
    assertEquals(eval("lower(s)", full), VStr("hello"))
    assertEquals(eval("starts_with(s, \"He\")", full), VBool(true))
    assertEquals(eval("contains(s, \"z\")", full), VBool(false))
  }

  test("missing, present and coalesce never return missing for present arguments") {
    assertEquals(eval("missing(a)", empty), VBool(true))
    assertEquals(eval("present(a)", full), VBool(true))
    assertEquals(eval("coalesce(a, b, 3)", empty), VNum(3))
    assertEquals(eval("coalesce(a, 3)", full), VNum(10))
  }

  test("string comparison is lexicographic") {
    assertEquals(eval("s < \"Z\"", full), VBool(true))
    assertEquals(eval("s >= \"Help\"", full), VBool(false))
  }

  test("a value of the wrong runtime type is treated as missing") {
    val wrong = FeatureVector(Map("a" -> FeatureValue.Str("oops")))
    assertEquals(eval("a > 1", wrong), Missing)
  }

  test("non-finite feature values are treated as missing") {
    val nan = FeatureVector(Map("a" -> FeatureValue.Num(Double.NaN)))
    assertEquals(eval("a", nan), Missing)
  }
