package canopy.rules

/** 1-based source position. */
final case class Pos(line: Int, col: Int):
  override def toString: String = s"$line:$col"

enum UnaryOp(val symbol: String):
  case Not extends UnaryOp("not")
  case Neg extends UnaryOp("-")

enum BinaryOp(val symbol: String, val precedence: Int):
  case Or extends BinaryOp("or", 1)
  case And extends BinaryOp("and", 2)
  case Eq extends BinaryOp("==", 4)
  case Ne extends BinaryOp("!=", 4)
  case Lt extends BinaryOp("<", 4)
  case Le extends BinaryOp("<=", 4)
  case Gt extends BinaryOp(">", 4)
  case Ge extends BinaryOp(">=", 4)
  case Add extends BinaryOp("+", 5)
  case Sub extends BinaryOp("-", 5)
  case Mul extends BinaryOp("*", 6)
  case Div extends BinaryOp("/", 6)
  case Mod extends BinaryOp("%", 6)

  def isComparison: Boolean = precedence == 4
  def isArithmetic: Boolean = precedence >= 5
  def isLogical: Boolean = precedence <= 2

/** Untyped expression tree produced by the parser. */
enum Expr:
  case NumLit(value: Double, pos: Pos)
  case StrLit(value: String, pos: Pos)
  case BoolLit(value: Boolean, pos: Pos)
  case Ref(name: String, pos: Pos)
  case ListLit(items: List[Expr], pos: Pos)
  case Unary(op: UnaryOp, operand: Expr, pos: Pos)
  case Binary(op: BinaryOp, left: Expr, right: Expr, pos: Pos)
  case In(needle: Expr, haystack: Expr, negated: Boolean, pos: Pos)
  case Between(value: Expr, low: Expr, high: Expr, pos: Pos)
  case Call(function: String, args: List[Expr], pos: Pos)

  def pos: Pos

/** What a rule asks for when its condition holds. */
enum Action:
  case Approve
  case Review
  case Decline
  /** Additive adjustment to the model's log-odds. */
  case Score(delta: Double)

  def render: String = this match
    case Approve  => "approve"
    case Review   => "review"
    case Decline  => "decline"
    case Score(d) => if d >= 0 then s"score +${Printer.number(d)}" else s"score ${Printer.number(d)}"

  def isHard: Boolean = this match
    case Score(_) => false
    case _        => true

enum Decl:
  case Let(name: String, expr: Expr, pos: Pos)
  case Rule(
      name: String,
      priority: Int,
      condition: Expr,
      action: Action,
      reason: Option[String],
      tags: List[String],
      pos: Pos
  )

final case class Program(decls: List[Decl]):
  def lets: List[Decl.Let] = decls.collect { case l: Decl.Let => l }
  def rules: List[Decl.Rule] = decls.collect { case r: Decl.Rule => r }
