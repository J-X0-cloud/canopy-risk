package canopy.rules

class TyperSuite extends munit.FunSuite:
  private val typer = Typer(
    Map(
      "amount" -> Type.Num,
      "count" -> Type.Num,
      "country" -> Type.Str,
      "is_new" -> Type.Bool
    )
  )

  private def check(src: String) = Parser.parse(src).left.map(List(_)).flatMap(typer.check)

  private def errors(src: String): List[String] =
    check(src).swap.toOption.getOrElse(fail(s"expected type errors for: $src")).filter(_.isError).map(_.message)

  test("well-typed program passes and lets are resolved") {
    val result = check("let big = amount > 100\nrule r when big and country == \"US\" then review reason \"x\"")
    val (typed, warnings) = result.fold(ds => fail(ds.map(_.summary).mkString), identity)
    assertEquals(warnings, Nil)
    assertEquals(typed.rules.head.condition.features, Set("amount", "country"))
  }

  test("unknown names get a did-you-mean hint") {
    val msgs = errors("rule r when amout > 1 then approve")
    assertEquals(msgs, List("unknown feature or let 'amout'; did you mean 'amount'?"))
  }

  test("unknown functions get a hint too") {
    val msgs = errors("rule r when lowr(country) == \"us\" then approve")
    assert(msgs.head.contains("did you mean 'lower'"), msgs.toString)
  }

  test("condition must be boolean") {
    assertEquals(errors("rule r when amount + 1 then approve"), List("rule condition must be a boolean but is a number"))
  }

  test("operand type mismatches name the side that is wrong") {
    assert(errors("rule r when amount > \"x\" then approve").head.contains("right operand"))
    assert(errors("rule r when (count and is_new) then approve").head.contains("left operand of 'and'"))
  }

  test("equality requires matching types") {
    assert(errors("rule r when country == 1 then approve").head.contains("cannot compare"))
  }

  test("string ordering comparisons are allowed") {
    assert(check("rule r when country < \"M\" then approve").isRight)
  }

  test("membership element types must agree") {
    assert(errors("rule r when country in [1, 2] then approve").head.contains("membership"))
    assert(errors("rule r when amount in [1, \"a\"] then approve").head.contains("list elements"))
    assert(errors("rule r when amount in [] then approve").head.contains("empty list"))
  }

  test("substring membership for strings") {
    assert(check("rule r when \"U\" in country then approve").isRight)
  }

  test("reversed literal between bounds are an error") {
    assert(errors("rule r when amount between 10 and 1 then approve").head.contains("reversed"))
  }

  test("function arity and argument types are checked") {
    assert(errors("rule r when abs(1, 2) > 0 then approve").head.contains("expects 1"))
    assert(errors("rule r when max(1) > 0 then approve").head.contains("at least 2"))
    assert(errors("rule r when coalesce(amount, \"x\") > 0 then approve").head.contains("same type"))
  }

  test("duplicate rules, duplicate lets and shadowing are rejected") {
    val msgs = errors(
      """let a = 1
        |let a = 2
        |let amount = 3
        |rule r when true then approve
        |rule r when true then approve
        |""".stripMargin
    )
    assertEquals(msgs.size, 3)
  }

  test("a let may only refer to earlier lets") {
    assert(errors("let a = b > 1\nlet b = 2\nrule r when a then approve").exists(_.contains("'b'")))
  }

  test("hard actions require a reason") {
    assert(errors("rule r when is_new then decline").head.contains("must give a reason"))
  }

  test("all errors are collected, not just the first") {
    val msgs = errors("rule a when nope then approve\nrule b when 1 then approve")
    assertEquals(msgs.size, 2)
  }

  test("lints are warnings and do not fail compilation") {
    val (_, warnings) = check("rule a when true then score 0").toOption.get
    assertEquals(warnings.map(_.severity).distinct, List(Severity.Warning))
    assertEquals(warnings.size, 2)
  }

  test("levenshtein distance") {
    assertEquals(Typer.levenshtein("kitten", "sitting"), 3)
    assertEquals(Typer.levenshtein("", "abc"), 3)
    assertEquals(Typer.levenshtein("same", "same"), 0)
  }
