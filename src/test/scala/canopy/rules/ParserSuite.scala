package canopy.rules

class ParserSuite extends munit.FunSuite:
  private def parseExpr(src: String): Expr =
    Parser.parseExpr(src).fold(d => fail(d.render(src)), identity)

  private def show(src: String): String = Printer.expr(parseExpr(src))

  test("multiplication binds tighter than addition") {
    parseExpr("1 + 2 * 3") match
      case Expr.Binary(BinaryOp.Add, Expr.NumLit(1, _), Expr.Binary(BinaryOp.Mul, _, _, _), _) => ()
      case other => fail(s"unexpected tree $other")
  }

  test("subtraction is left associative") {
    parseExpr("10 - 4 - 3") match
      case Expr.Binary(BinaryOp.Sub, Expr.Binary(BinaryOp.Sub, _, _, _), Expr.NumLit(3, _), _) => ()
      case other => fail(s"unexpected tree $other")
  }

  test("and binds tighter than or, not binds looser than comparison") {
    assertEquals(show("a or b and c"), "a or b and c")
    parseExpr("not x > 1") match
      case Expr.Unary(UnaryOp.Not, Expr.Binary(BinaryOp.Gt, _, _, _), _) => ()
      case other => fail(s"unexpected tree $other")
  }

  test("between consumes its own 'and' and leaves the logical one alone") {
    parseExpr("x between 1 and 5 and y") match
      case Expr.Binary(BinaryOp.And, Expr.Between(_, _, _, _), Expr.Ref("y", _), _) => ()
      case other => fail(s"unexpected tree $other")
  }

  test("not in parses as a negated membership test") {
    parseExpr("country not in [\"US\", \"CA\"]") match
      case Expr.In(Expr.Ref("country", _), Expr.ListLit(items, _), true, _) =>
        assertEquals(items.size, 2)
      case other => fail(s"unexpected tree $other")
  }

  test("chained comparisons are rejected with a helpful message") {
    val err = Parser.parseExpr("1 < x < 3").swap.toOption.get
    assert(err.message.contains("cannot be chained"), err.message)
    assertEquals(err.pos, Pos(1, 7))
  }

  test("unary minus binds tighter than multiplication") {
    parseExpr("-a * b") match
      case Expr.Binary(BinaryOp.Mul, Expr.Unary(UnaryOp.Neg, _, _), _, _) => ()
      case other => fail(s"unexpected tree $other")
  }

  test("function calls with zero and many arguments") {
    parseExpr("f()") match
      case Expr.Call("f", Nil, _) => ()
      case other                  => fail(s"unexpected tree $other")
    parseExpr("max(a, b, 3)") match
      case Expr.Call("max", List(_, _, _), _) => ()
      case other                              => fail(s"unexpected tree $other")
  }

  test("full rule declaration with every clause") {
    val src =
      """let small = amount_usd < 5
        |
        |rule card_testing priority 90
        |  when small and card.count_10m >= 5
        |  then decline
        |  reason "Burst of small authorizations"
        |  tag "card-testing"
        |  tag "velocity"
        |""".stripMargin
    val program = Parser.parse(src).fold(d => fail(d.render(src)), identity)
    assertEquals(program.lets.map(_.name), List("small"))
    val rule = program.rules.head
    assertEquals(rule.name, "card_testing")
    assertEquals(rule.priority, 90)
    assertEquals(rule.action, Action.Decline)
    assertEquals(rule.reason, Some("Burst of small authorizations"))
    assertEquals(rule.tags, List("card-testing", "velocity"))
    assertEquals(rule.pos, Pos(3, 1))
  }

  test("score actions accept explicit signs") {
    val program = Parser.parse("rule a when true then score -0.75\nrule b when true then score +2").toOption.get
    assertEquals(program.rules.map(_.action), List(Action.Score(-0.75), Action.Score(2)))
  }

  test("errors point at the offending token") {
    val src = "rule r\n  when x >\n  then review"
    val err = Parser.parse(src).swap.toOption.get
    assertEquals(err.pos, Pos(3, 3))
    assert(err.message.contains("expected an expression"), err.message)
    assert(err.render(src).contains("^"))
  }

  test("reserved words cannot name rules and rule names cannot contain dots") {
    assert(Parser.parse("rule when when true then approve").swap.toOption.get.message.contains("reserved"))
    assert(Parser.parse("rule a.b when true then approve").swap.toOption.get.message.contains("'.'"))
  }

  test("fractional priority is rejected") {
    val err = Parser.parse("rule r priority 1.5 when true then approve").swap.toOption.get
    assert(err.message.contains("priority"), err.message)
  }

  test("missing action is reported") {
    val err = Parser.parse("rule r when true then").swap.toOption.get
    assert(err.message.contains("an action"), err.message)
  }

  test("printer inserts parentheses only where the tree needs them") {
    assertEquals(show("(a + b) * c"), "(a + b) * c")
    assertEquals(show("a + (b * c)"), "a + b * c")
    assertEquals(show("a - (b - c)"), "a - (b - c)")
    assertEquals(show("(a or b) and c"), "(a or b) and c")
    assertEquals(show("not (a and b)"), "not (a and b)")
    assertEquals(show("(not a) == b"), "(not a) == b")
    assertEquals(show("-(a + b)"), "-(a + b)")
    assertEquals(show("(x > 1) == true"), "(x > 1) == true")
  }

  test("print then parse is a fixed point for a full program") {
    val src =
      """let risky = merchant.mcc in [7995, 6051] or contains(lower(channel), "app")
        |rule r1 priority 5 when risky and amount_usd between 10 and 1e6 then review reason "a \"quoted\" reason"
        |rule r2 when not (card.is_new or x != 1) then score -1.5 tag "t"
        |""".stripMargin
    val first = Printer.program(Parser.parse(src).toOption.get)
    val second = Printer.program(Parser.parse(first).fold(d => fail(d.render(first)), identity))
    assertEquals(second, first)
  }
