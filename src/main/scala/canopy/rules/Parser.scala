package canopy.rules

import scala.util.control.NoStackTrace

/**
 * Recursive-descent parser with Pratt-style precedence climbing for expressions.
 *
 * {{{
 * program   := decl* EOF
 * decl      := "let" IDENT "=" expr
 *            | "rule" IDENT ("priority" INT)? "when" expr "then" action
 *              ("reason" STRING)? ("tag" STRING)*
 * action    := "approve" | "review" | "decline" | "score" ("+" | "-")? NUMBER
 * expr      := precedence climbing over
 *              or < and < not < (comparison | in | not in | between) < (+ -) < (* / %) < unary -
 * primary   := NUMBER | STRING | "true" | "false" | IDENT | IDENT "(" args ")"
 *            | "[" (expr ("," expr)*)? "]" | "(" expr ")"
 * }}}
 *
 * Comparisons are non-associative: `a < b < c` is rejected instead of being read as `(a < b) < c`.
 */
object Parser:
  def parse(source: String): Either[Diagnostic, Program] =
    Lexer.tokenize(source).flatMap(tokens => new Parser(tokens).program())

  def parseExpr(source: String): Either[Diagnostic, Expr] =
    Lexer.tokenize(source).flatMap(tokens => new Parser(tokens).standaloneExpr())

final private class Parser(tokens: Vector[Token]):
  final private case class Failure(diagnostic: Diagnostic) extends Exception with NoStackTrace

  private var index = 0

  private val NotPrecedence = 3
  private val ComparisonPrecedence = 4
  private val UnaryMinusPrecedence = 7

  private def peek: Token = tokens(index)
  private def peekAt(offset: Int): Tok = tokens(math.min(index + offset, tokens.size - 1)).tok
  private def advance(): Token =
    val t = tokens(index)
    if index < tokens.size - 1 then index += 1
    t

  private def fail(msg: String, pos: Pos = peek.pos): Nothing =
    throw Failure(Diagnostic(Severity.Error, msg, pos))

  private def expected(what: String): Nothing =
    fail(s"expected $what but found ${peek.tok.describe}")

  private def acceptKw(word: String): Boolean =
    if peek.tok == Tok.Kw(word) then
      advance(); true
    else false

  private def acceptSym(sym: String): Boolean =
    if peek.tok == Tok.Sym(sym) then
      advance(); true
    else false

  private def expectKw(word: String): Pos =
    val t = peek
    if !acceptKw(word) then expected(s"'$word'")
    t.pos

  private def expectSym(sym: String): Pos =
    val t = peek
    if !acceptSym(sym) then expected(s"'$sym'")
    t.pos

  private def run[A](body: => A): Either[Diagnostic, A] =
    try Right(body)
    catch case Failure(d) => Left(d)

  def program(): Either[Diagnostic, Program] = run {
    val decls = List.newBuilder[Decl]
    while peek.tok != Tok.Eof do decls += decl()
    Program(decls.result())
  }

  def standaloneExpr(): Either[Diagnostic, Expr] = run {
    val e = expr(0)
    if peek.tok != Tok.Eof then expected("end of expression")
    e
  }

  private def decl(): Decl = peek.tok match
    case Tok.Kw("let")  => letDecl()
    case Tok.Kw("rule") => ruleDecl()
    case _              => expected("'rule' or 'let'")

  private def simpleName(what: String): String =
    peek.tok match
      case Tok.Ident(name) if !name.contains('.') =>
        advance()
        name
      case Tok.Ident(name) => fail(s"$what '$name' may not contain '.'")
      case Tok.Kw(word)    => fail(s"'$word' is a reserved word and cannot be used as a $what")
      case _               => expected(s"a $what")

  private def letDecl(): Decl =
    val pos = expectKw("let")
    val name = simpleName("let name")
    expectSym("=")
    Decl.Let(name, expr(0), pos)

  private def ruleDecl(): Decl =
    val pos = expectKw("rule")
    val name = simpleName("rule name")
    val priority =
      if acceptKw("priority") then
        peek.tok match
          case Tok.Num(v, _) if v.isWhole && v <= Int.MaxValue =>
            advance()
            v.toInt
          case Tok.Num(_, text) => fail(s"priority must be a non-negative integer, got $text")
          case _                => expected("an integer priority")
      else 0
    expectKw("when")
    val condition = expr(0)
    expectKw("then")
    val action = actionClause()
    val reason =
      if acceptKw("reason") then
        peek.tok match
          case Tok.Str(s) =>
            advance()
            Some(s)
          case _ => expected("a reason string")
      else None
    val tags = List.newBuilder[String]
    while acceptKw("tag") do
      peek.tok match
        case Tok.Str(s) if s.nonEmpty =>
          advance()
          tags += s
        case Tok.Str(_) => fail("tag must not be empty")
        case _          => expected("a tag string")
    Decl.Rule(name, priority, condition, action, reason, tags.result(), pos)

  private def actionClause(): Action = peek.tok match
    case Tok.Kw("approve") => advance(); Action.Approve
    case Tok.Kw("review")  => advance(); Action.Review
    case Tok.Kw("decline") => advance(); Action.Decline
    case Tok.Kw("score") =>
      advance()
      val sign =
        if acceptSym("-") then -1.0
        else
          acceptSym("+")
          1.0
      peek.tok match
        case Tok.Num(v, _) =>
          advance()
          Action.Score(sign * v)
        case _ => expected("a numeric score adjustment")
    case _ => expected("an action (approve, review, decline or score)")

  /** Precedence climbing. Parses an expression whose operators all bind at least `minPrec`. */
  private def expr(minPrec: Int): Expr =
    var left = prefix()
    var continue = true
    while continue do
      val start = peek
      infixOperator() match
        case Some(op) if op.precedence >= minPrec =>
          advance()
          op match
            case BinaryOp.Or | BinaryOp.And =>
              left = Expr.Binary(op, left, expr(op.precedence + 1), start.pos)
            case cmp if cmp.isComparison =>
              val right = expr(cmp.precedence + 1)
              left = Expr.Binary(cmp, left, right, start.pos)
              rejectChainedComparison()
            case arith =>
              left = Expr.Binary(arith, left, expr(arith.precedence + 1), start.pos)
        case None if ComparisonPrecedence >= minPrec && isMembership =>
          val negated = acceptKw("not")
          expectKw("in")
          left = Expr.In(left, expr(ComparisonPrecedence + 1), negated, start.pos)
          rejectChainedComparison()
        case None if ComparisonPrecedence >= minPrec && peek.tok == Tok.Kw("between") =>
          advance()
          val low = expr(ComparisonPrecedence + 1)
          expectKw("and")
          val high = expr(ComparisonPrecedence + 1)
          left = Expr.Between(left, low, high, start.pos)
          rejectChainedComparison()
        case _ => continue = false
    left

  private def isMembership: Boolean =
    peek.tok == Tok.Kw("in") || (peek.tok == Tok.Kw("not") && peekAt(1) == Tok.Kw("in"))

  private def rejectChainedComparison(): Unit =
    if infixOperator().exists(_.isComparison) || isMembership || peek.tok == Tok.Kw("between") then
      fail("comparison operators cannot be chained; combine them with 'and'")

  private def infixOperator(): Option[BinaryOp] = peek.tok match
    case Tok.Kw("or")  => Some(BinaryOp.Or)
    case Tok.Kw("and") => Some(BinaryOp.And)
    case Tok.Sym(s)    => BinaryOp.values.find(op => op.symbol == s)
    case _             => None

  private def prefix(): Expr =
    val t = peek
    t.tok match
      case Tok.Kw("not") =>
        advance()
        Expr.Unary(UnaryOp.Not, expr(NotPrecedence), t.pos)
      case Tok.Sym("-") =>
        advance()
        Expr.Unary(UnaryOp.Neg, expr(UnaryMinusPrecedence), t.pos)
      case _ => primary()

  private def primary(): Expr =
    val t = advance()
    t.tok match
      case Tok.Num(v, _)   => Expr.NumLit(v, t.pos)
      case Tok.Str(s)      => Expr.StrLit(s, t.pos)
      case Tok.Kw("true")  => Expr.BoolLit(true, t.pos)
      case Tok.Kw("false") => Expr.BoolLit(false, t.pos)
      case Tok.Ident(name) if peek.tok == Tok.Sym("(") =>
        advance()
        Expr.Call(name, delimited(")"), t.pos)
      case Tok.Ident(name) => Expr.Ref(name, t.pos)
      case Tok.Sym("[")    => Expr.ListLit(delimited("]"), t.pos)
      case Tok.Sym("(") =>
        val inner = expr(0)
        expectSym(")")
        inner
      case other => fail(s"expected an expression but found ${other.describe}", t.pos)

  /** Comma-separated expressions up to `close`; the opening bracket is already consumed. */
  private def delimited(close: String): List[Expr] =
    if acceptSym(close) then Nil
    else
      val items = List.newBuilder[Expr]
      items += expr(0)
      while acceptSym(",") do items += expr(0)
      expectSym(close)
      items.result()
