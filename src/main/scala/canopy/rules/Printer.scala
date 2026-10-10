package canopy.rules

/**
 * Renders syntax trees back to source with the minimum parentheses needed to preserve meaning.
 * `Parser.parse(Printer.program(p))` yields a program equal to `p` up to source positions.
 */
object Printer:
  private val AtomPrecedence = 100

  def number(d: Double): String =
    if d.isWhole && math.abs(d) < 1e15 then d.toLong.toString else d.toString

  def string(s: String): String =
    val escaped = s.flatMap {
      case '"'  => "\\\""
      case '\\' => "\\\\"
      case '\n' => "\\n"
      case '\t' => "\\t"
      case c    => c.toString
    }
    s"\"$escaped\""

  private def precedence(e: Expr): Int = e match
    case Expr.Unary(UnaryOp.Neg, _, _) => 7
    case Expr.Unary(UnaryOp.Not, _, _) => 3
    case Expr.Binary(op, _, _, _)      => op.precedence
    case _: Expr.In | _: Expr.Between  => 4
    case Expr.NumLit(v, _) if v < 0    => 7
    case _                             => AtomPrecedence

  private def at(e: Expr, required: Int): String =
    val s = expr(e)
    if precedence(e) < required then s"($s)" else s

  def expr(e: Expr): String = e match
    case Expr.NumLit(v, _)                   => number(v)
    case Expr.StrLit(s, _)                   => string(s)
    case Expr.BoolLit(b, _)                  => b.toString
    case Expr.Ref(name, _)                   => name
    case Expr.ListLit(xs, _)                 => xs.map(expr).mkString("[", ", ", "]")
    case Expr.Call(f, args, _)               => args.map(expr).mkString(s"$f(", ", ", ")")
    case Expr.Unary(UnaryOp.Neg, operand, _) => s"-${at(operand, 7)}"
    case Expr.Unary(UnaryOp.Not, operand, _) => s"not ${at(operand, 3)}"
    case Expr.Binary(op, l, r, _) =>
      val leftReq = if op.isComparison then op.precedence + 1 else op.precedence
      s"${at(l, leftReq)} ${op.symbol} ${at(r, op.precedence + 1)}"
    case Expr.In(needle, haystack, negated, _) =>
      val kw = if negated then "not in" else "in"
      s"${at(needle, 5)} $kw ${at(haystack, 5)}"
    case Expr.Between(v, lo, hi, _) => s"${at(v, 5)} between ${at(lo, 5)} and ${at(hi, 5)}"

  def decl(d: Decl): String = d match
    case Decl.Let(name, e, _) => s"let $name = ${expr(e)}"
    case Decl.Rule(name, priority, cond, action, reason, tags, _) =>
      val header = if priority != 0 then s"rule $name priority $priority" else s"rule $name"
      val lines = List(header, s"  when ${expr(cond)}", s"  then ${action.render}") ++
        reason.map(r => s"  reason ${string(r)}") ++
        tags.map(t => s"  tag ${string(t)}")
      lines.mkString("\n")

  def program(p: Program): String = p.decls.map(decl).mkString("", "\n\n", "\n")
