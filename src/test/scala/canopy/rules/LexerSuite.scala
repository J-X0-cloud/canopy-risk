package canopy.rules

class LexerSuite extends munit.FunSuite:
  private def toks(src: String): List[Tok] =
    Lexer.tokenize(src).fold(d => fail(d.summary), _.map(_.tok).toList)

  test("dotted identifiers are single tokens and keywords are recognised") {
    assertEquals(
      toks("card.count_1h >= 3 and not x"),
      List(
        Tok.Ident("card.count_1h"),
        Tok.Sym(">="),
        Tok.Num(3, "3"),
        Tok.Kw("and"),
        Tok.Kw("not"),
        Tok.Ident("x"),
        Tok.Eof
      )
    )
  }

  test("numbers support decimals, exponents and digit separators") {
    assertEquals(toks("1_000 2.5 1e3 4.5E-2"), List(
      Tok.Num(1000, "1_000"),
      Tok.Num(2.5, "2.5"),
      Tok.Num(1000, "1e3"),
      Tok.Num(0.045, "4.5E-2"),
      Tok.Eof
    ))
  }

  test("a trailing 'e' without digits is not swallowed into the number") {
    val result = Lexer.tokenize("3e")
    assert(result.isLeft, result)
  }

  test("misplaced digit separators are rejected") {
    List("1__0", "1_", "1_.5").foreach { src =>
      assert(Lexer.tokenize(src).isLeft, src)
    }
  }

  test("string escapes are decoded") {
    assertEquals(toks("\"a\\\"b\\\\c\\n\""), List(Tok.Str("a\"b\\c\n"), Tok.Eof))
  }

  test("unterminated strings report the opening quote position") {
    val err = Lexer.tokenize("x == \"abc").swap.toOption.get
    assertEquals(err.pos, Pos(1, 6))
    assert(err.message.contains("unterminated"))
  }

  test("comments are skipped and positions track lines and columns") {
    val tokens = Lexer.tokenize("# header\n  rule r # trailing\nwhen").toOption.get
    assertEquals(tokens.map(_.pos).take(3), Vector(Pos(2, 3), Pos(2, 8), Pos(3, 1)))
  }

  test("a dangling dot in an identifier is an error at the dot") {
    val err = Lexer.tokenize("card. > 1").swap.toOption.get
    assertEquals(err.pos, Pos(1, 6))
  }

  test("unknown characters are rejected") {
    val err = Lexer.tokenize("a $ b").swap.toOption.get
    assertEquals(err.pos, Pos(1, 3))
  }

  test("two-character operators win over their one-character prefixes") {
    assertEquals(toks("<= < == = !="), List(
      Tok.Sym("<="), Tok.Sym("<"), Tok.Sym("=="), Tok.Sym("="), Tok.Sym("!="), Tok.Eof
    ))
  }
