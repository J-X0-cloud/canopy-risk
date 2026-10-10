package canopy.rules

import canopy.features.{FeatureValue, FeatureVector}

/** Runtime value. `Missing` doubles as the "unknown" truth value of three-valued logic. */
enum Value:
  case VNum(value: Double)
  case VBool(value: Boolean)
  case VStr(value: String)
  case VList(items: Vector[Value])
  case Missing

  def render: String = this match
    case VNum(v)   => Printer.number(v)
    case VBool(b)  => b.toString
    case VStr(s)   => Printer.string(s)
    case VList(xs) => xs.map(_.render).mkString("[", ", ", "]")
    case Missing   => "missing"

/**
 * Compiles typed expressions into closures over a feature vector.
 *
 * Semantics for absent data follow SQL's three-valued logic: arithmetic or comparison involving a
 * missing value is missing, `and`/`or` use Kleene truth tables (`false and missing` is false,
 * `true or missing` is true), and a rule fires only when its condition is definitely true. Division
 * by zero and non-finite results are missing rather than exceptions or infinities.
 */
object Evaluator:
  import Value.*

  type Compiled = FeatureVector => Value

  private def num(d: Double): Value = if d.isNaN || d.isInfinite then Missing else VNum(d)

  def compile(t: Typed): Compiled = t match
    case Typed.NumLit(v) =>
      val r = VNum(v); _ => r
    case Typed.StrLit(s) =>
      val r = VStr(s); _ => r
    case Typed.BoolLit(b) =>
      val r = VBool(b); _ => r
    case Typed.Feature(name, tpe) => fv => fromFeature(fv.get(name), tpe)
    case Typed.LetRef(_, body)    => compile(body)
    case Typed.ListLit(items, _) =>
      val cs = items.map(compile).toVector
      fv => VList(cs.map(_(fv)))
    case Typed.Unary(UnaryOp.Not, e, _) =>
      val c = compile(e)
      fv =>
        c(fv) match
          case VBool(b) => VBool(!b)
          case _        => Missing
    case Typed.Unary(UnaryOp.Neg, e, _) =>
      val c = compile(e)
      fv =>
        c(fv) match
          case VNum(v) => VNum(-v)
          case _       => Missing
    case Typed.Binary(BinaryOp.And, l, r, _) =>
      val (cl, cr) = (compile(l), compile(r))
      fv =>
        cl(fv) match
          case VBool(false) => VBool(false)
          case left =>
            (left, cr(fv)) match
              case (_, VBool(false))          => VBool(false)
              case (VBool(true), VBool(true)) => VBool(true)
              case _                          => Missing
    case Typed.Binary(BinaryOp.Or, l, r, _) =>
      val (cl, cr) = (compile(l), compile(r))
      fv =>
        cl(fv) match
          case VBool(true) => VBool(true)
          case left =>
            (left, cr(fv)) match
              case (_, VBool(true))             => VBool(true)
              case (VBool(false), VBool(false)) => VBool(false)
              case _                            => Missing
    case Typed.Binary(op, l, r, _) =>
      val (cl, cr) = (compile(l), compile(r))
      val f = binary(op)
      fv =>
        (cl(fv), cr(fv)) match
          case (Missing, _) | (_, Missing) => Missing
          case (a, b)                      => f(a, b)
    case Typed.In(n, h, negated) =>
      val (cn, ch) = (compile(n), compile(h))
      fv =>
        val result = (cn(fv), ch(fv)) match
          case (Missing, _) | (_, Missing) => Missing
          case (VStr(needle), VStr(hay))   => VBool(hay.contains(needle))
          case (needle, VList(items)) =>
            if items.contains(needle) then VBool(true)
            else if items.contains(Missing) then Missing
            else VBool(false)
          case _ => Missing
        if negated then
          result match
            case VBool(b) => VBool(!b)
            case other    => other
        else result
    case Typed.Between(v, lo, hi) =>
      val (cv, clo, chi) = (compile(v), compile(lo), compile(hi))
      fv =>
        (cv(fv), clo(fv), chi(fv)) match
          case (VNum(x), VNum(a), VNum(b)) => VBool(a <= x && x <= b)
          case _                           => Missing
    case Typed.Call(fn, args, _) => call(fn, args.map(compile).toVector)

  private def fromFeature(v: Option[FeatureValue], tpe: Type): Value = (v, tpe) match
    case (Some(FeatureValue.Num(d)), Type.Num)   => num(d)
    case (Some(FeatureValue.Bool(b)), Type.Bool) => VBool(b)
    case (Some(FeatureValue.Str(s)), Type.Str)   => VStr(s)
    case _                                       => Missing

  private def binary(op: BinaryOp): (Value, Value) => Value = op match
    case BinaryOp.Add => arith(_ + _)
    case BinaryOp.Sub => arith(_ - _)
    case BinaryOp.Mul => arith(_ * _)
    case BinaryOp.Div => arith((a, b) => if b == 0.0 then Double.NaN else a / b)
    case BinaryOp.Mod => arith((a, b) => if b == 0.0 then Double.NaN else a % b)
    case BinaryOp.Eq  => (a, b) => VBool(a == b)
    case BinaryOp.Ne  => (a, b) => VBool(a != b)
    case BinaryOp.Lt  => ordered(_ < 0)
    case BinaryOp.Le  => ordered(_ <= 0)
    case BinaryOp.Gt  => ordered(_ > 0)
    case BinaryOp.Ge  => ordered(_ >= 0)
    case BinaryOp.And | BinaryOp.Or =>
      throw IllegalStateException("logical operators are compiled with short-circuiting")

  private def arith(f: (Double, Double) => Double): (Value, Value) => Value =
    case (VNum(a), VNum(b)) => num(f(a, b))
    case _                  => Missing

  private def ordered(test: Int => Boolean): (Value, Value) => Value =
    case (VNum(a), VNum(b)) => VBool(test(java.lang.Double.compare(a, b)))
    case (VStr(a), VStr(b)) => VBool(test(a.compareTo(b)))
    case _                  => Missing

  private def call(fn: Builtins.Fn, args: Vector[Compiled]): Compiled =
    import Builtins.Fn
    def numeric1(f: Double => Double): Compiled = fv =>
      args(0)(fv) match
        case VNum(v) => num(f(v))
        case _       => Missing
    fn match
      case Fn.Abs   => numeric1(math.abs)
      case Fn.Log1p => numeric1(v => if v <= -1 then Double.NaN else math.log1p(v))
      case Fn.Sqrt  => numeric1(v => if v < 0 then Double.NaN else math.sqrt(v))
      case Fn.Min | Fn.Max =>
        val pick: (Double, Double) => Double = if fn == Fn.Min then math.min else math.max
        fv =>
          val vs = args.map(_(fv))
          if vs.contains(Missing) then Missing
          else VNum(vs.collect { case VNum(v) => v }.reduce(pick))
      case Fn.Missing => fv => VBool(args(0)(fv) == Missing)
      case Fn.Present => fv => VBool(args(0)(fv) != Missing)
      case Fn.Coalesce =>
        fv => args.iterator.map(_(fv)).find(_ != Missing).getOrElse(Missing)
      case Fn.Lower =>
        fv =>
          args(0)(fv) match
            case VStr(s) => VStr(s.toLowerCase)
            case _       => Missing
      case Fn.StartsWith | Fn.Contains =>
        val test: (String, String) => Boolean =
          if fn == Fn.StartsWith then _.startsWith(_) else _.contains(_)
        fv =>
          (args(0)(fv), args(1)(fv)) match
            case (VStr(a), VStr(b)) => VBool(test(a, b))
            case _                  => Missing
