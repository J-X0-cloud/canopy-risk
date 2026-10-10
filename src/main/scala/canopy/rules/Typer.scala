package canopy.rules

import canopy.features.FeatureType

enum Type:
  case Num, Bool, Str
  case ListOf(elem: Type)

  def show: String = this match
    case Num       => "number"
    case Bool      => "boolean"
    case Str       => "string"
    case ListOf(e) => s"list of ${e.show}"

object Type:
  def of(ft: FeatureType): Type = ft match
    case FeatureType.Number  => Num
    case FeatureType.Boolean => Bool
    case FeatureType.Text    => Str

/** Expression annotated with its type, with `let` references resolved. */
enum Typed:
  case NumLit(value: Double)
  case StrLit(value: String)
  case BoolLit(value: Boolean)
  case Feature(name: String, ty: Type)
  case LetRef(name: String, body: Typed)
  case ListLit(items: List[Typed], ty: Type)
  case Unary(op: UnaryOp, operand: Typed, ty: Type)
  case Binary(op: BinaryOp, left: Typed, right: Typed, ty: Type)
  case In(needle: Typed, haystack: Typed, negated: Boolean)
  case Between(value: Typed, low: Typed, high: Typed)
  case Call(fn: Builtins.Fn, args: List[Typed], ty: Type)

  def tpe: Type = this match
    case NumLit(_)          => Type.Num
    case StrLit(_)          => Type.Str
    case BoolLit(_)         => Type.Bool
    case Feature(_, t)      => t
    case LetRef(_, body)    => body.tpe
    case ListLit(_, t)      => t
    case Unary(_, _, t)     => t
    case Binary(_, _, _, t) => t
    case In(_, _, _)        => Type.Bool
    case Between(_, _, _)   => Type.Bool
    case Call(_, _, t)      => t

  /** Features this expression reads, directly or through lets. */
  def features: Set[String] = this match
    case Feature(n, _)                      => Set(n)
    case LetRef(_, b)                       => b.features
    case ListLit(xs, _)                     => xs.flatMap(_.features).toSet
    case Unary(_, e, _)                     => e.features
    case Binary(_, l, r, _)                 => l.features ++ r.features
    case In(n, h, _)                        => n.features ++ h.features
    case Between(v, lo, hi)                 => v.features ++ lo.features ++ hi.features
    case Call(_, args, _)                   => args.flatMap(_.features).toSet
    case NumLit(_) | StrLit(_) | BoolLit(_) => Set.empty

final case class TypedRule(rule: Decl.Rule, condition: Typed)

final case class TypedProgram(rules: List[TypedRule], lets: Map[String, Typed])

/**
 * Static checks for a parsed program: name resolution, operator and function typing, duplicate
 * names, and a few policy lints. All problems are collected rather than stopping at the first.
 */
final class Typer(schema: Map[String, Type]):

  final private case class Ctx(lets: Map[String, Typed])

  def check(program: Program): Either[List[Diagnostic], (TypedProgram, List[Diagnostic])] =
    val diags = List.newBuilder[Diagnostic]
    def error(msg: String, pos: Pos): Unit = diags += Diagnostic(Severity.Error, msg, pos)
    def warn(msg: String, pos: Pos): Unit = diags += Diagnostic(Severity.Warning, msg, pos)

    var ctx = Ctx(Map.empty)
    val typedRules = List.newBuilder[TypedRule]
    var ruleNames = Set.empty[String]

    program.decls.foreach {
      case Decl.Let(name, e, pos) =>
        if schema.contains(name) then
          error(s"let '$name' would shadow the feature of that name", pos)
        else if ctx.lets.contains(name) then error(s"let '$name' is already defined", pos)
        else
          infer(e, ctx, error) match
            case Some(t) => ctx = ctx.copy(lets = ctx.lets.updated(name, t))
            case None    => ()
      case rule @ Decl.Rule(name, _, cond, action, reason, _, pos) =>
        if ruleNames.contains(name) then error(s"rule '$name' is defined more than once", pos)
        ruleNames += name
        infer(cond, ctx, error).foreach { t =>
          if t.tpe != Type.Bool then
            error(s"rule condition must be a boolean but is a ${t.tpe.show}", cond.pos)
          else
            constant(t).foreach(v => warn(s"rule '$name' condition is always $v", cond.pos))
            typedRules += TypedRule(rule, t)
        }
        action match
          case Action.Decline | Action.Review if reason.isEmpty =>
            error(s"rule '$name' must give a reason for a ${action.render} action", pos)
          case Action.Score(d) if d == 0.0 => warn(s"rule '$name' adjusts the score by zero", pos)
          case Action.Score(d) if math.abs(d) > 10 =>
            warn(s"rule '$name' score adjustment $d saturates the probability", pos)
          case _ => ()
    }

    val all = diags.result()
    if all.exists(_.isError) then Left(all.filter(_.isError) ++ all.filterNot(_.isError))
    else Right((TypedProgram(typedRules.result(), ctx.lets), all))

  /** Evaluates literal-only boolean expressions so trivially constant rules can be flagged. */
  private def constant(t: Typed): Option[Boolean] = t match
    case Typed.BoolLit(b)               => Some(b)
    case Typed.Unary(UnaryOp.Not, e, _) => constant(e).map(!_)
    case _                              => None

  def inferExpr(e: Expr): Either[List[Diagnostic], Typed] =
    val errs = List.newBuilder[Diagnostic]
    infer(e, Ctx(Map.empty), (m, p) => errs += Diagnostic(Severity.Error, m, p)) match
      case Some(t) => Right(t)
      case None    => Left(errs.result())

  private def infer(e: Expr, ctx: Ctx, error: (String, Pos) => Unit): Option[Typed] =
    def go(e: Expr): Option[Typed] = infer(e, ctx, error)

    def expect(t: Typed, want: Type, what: String, pos: Pos): Option[Typed] =
      if t.tpe == want then Some(t)
      else
        error(s"$what must be a ${want.show} but is a ${t.tpe.show}", pos)
        None

    e match
      case Expr.NumLit(v, _)  => Some(Typed.NumLit(v))
      case Expr.StrLit(s, _)  => Some(Typed.StrLit(s))
      case Expr.BoolLit(b, _) => Some(Typed.BoolLit(b))
      case Expr.Ref(name, pos) =>
        ctx.lets.get(name).map(Typed.LetRef(
          name,
          _
        )).orElse(schema.get(name).map(Typed.Feature(name, _))) match
          case found @ Some(_) => found
          case None =>
            val hint = Typer.suggest(name, schema.keySet ++ ctx.lets.keySet)
              .fold("")(s => s"; did you mean '$s'?")
            error(s"unknown feature or let '$name'$hint", pos)
            None
      case Expr.ListLit(Nil, pos) =>
        error("empty list literal has no element type", pos)
        None
      case Expr.ListLit(items, pos) =>
        val typed = items.map(go)
        if typed.exists(_.isEmpty) then None
        else
          val ts = typed.flatten
          val elem = ts.head.tpe
          ts.zip(items).find(_._1.tpe != elem) match
            case Some((bad, at)) =>
              error(s"list elements must all be ${elem.show}, found a ${bad.tpe.show}", at.pos)
              None
            case None if elem.isInstanceOf[Type.ListOf] =>
              error("nested lists are not supported", pos)
              None
            case None => Some(Typed.ListLit(ts, Type.ListOf(elem)))
      case Expr.Unary(UnaryOp.Not, operand, _) =>
        go(operand)
          .flatMap(expect(_, Type.Bool, "operand of 'not'", operand.pos))
          .map(Typed.Unary(UnaryOp.Not, _, Type.Bool))
      case Expr.Unary(UnaryOp.Neg, operand, _) =>
        go(operand)
          .flatMap(expect(_, Type.Num, "operand of unary '-'", operand.pos))
          .map(Typed.Unary(UnaryOp.Neg, _, Type.Num))
      case Expr.Binary(op, l, r, pos) =>
        val (lt, rt) = (go(l), go(r))
        for
          a <- lt
          b <- rt
          result <- binary(op, a, b, l, r, pos, error)
        yield result
      case Expr.In(needle, haystack, negated, pos) =>
        for
          n <- go(needle)
          h <- go(haystack)
          ok <- h.tpe match
            case Type.ListOf(elem) if elem == n.tpe => Some(Typed.In(n, h, negated))
            case Type.ListOf(elem) =>
              error(s"cannot test a ${n.tpe.show} for membership in a list of ${elem.show}", pos)
              None
            case Type.Str if n.tpe == Type.Str => Some(Typed.In(n, h, negated))
            case other =>
              error(
                s"right-hand side of 'in' must be a list or string, not a ${other.show}",
                haystack.pos
              )
              None
        yield ok
      case Expr.Between(v, lo, hi, _) =>
        val parts = List(v -> "value", lo -> "lower bound", hi -> "upper bound").map { (x, what) =>
          go(x).flatMap(expect(_, Type.Num, s"$what of 'between'", x.pos))
        }
        parts match
          case List(Some(a), Some(b), Some(c)) =>
            (b, c) match
              case (Typed.NumLit(x), Typed.NumLit(y)) if x > y =>
                error(s"'between' bounds are reversed: $x is greater than $y", lo.pos)
                None
              case _ => Some(Typed.Between(a, b, c))
          case _ => None
      case Expr.Call(name, args, pos) =>
        val typedArgs = args.map(go)
        if typedArgs.exists(_.isEmpty) then None
        else
          Builtins.lookup(name) match
            case None =>
              val hint = Typer.suggest(name, Builtins.names).fold("")(s => s"; did you mean '$s'?")
              error(s"unknown function '$name'$hint", pos)
              None
            case Some(fn) =>
              val ts = typedArgs.flatten
              fn.resultType(ts.map(_.tpe)) match
                case Right(t) => Some(Typed.Call(fn, ts, t))
                case Left(msg) =>
                  error(s"$name: $msg", pos)
                  None

  private def binary(
      op: BinaryOp,
      a: Typed,
      b: Typed,
      l: Expr,
      r: Expr,
      pos: Pos,
      error: (String, Pos) => Unit
  ): Option[Typed] =
    def both(t: Type, result: Type): Option[Typed] =
      if a.tpe != t then
        error(s"left operand of '${op.symbol}' must be a ${t.show} but is a ${a.tpe.show}", l.pos)
        None
      else if b.tpe != t then
        error(s"right operand of '${op.symbol}' must be a ${t.show} but is a ${b.tpe.show}", r.pos)
        None
      else Some(Typed.Binary(op, a, b, result))
    op match
      case BinaryOp.And | BinaryOp.Or => both(Type.Bool, Type.Bool)
      case BinaryOp.Eq | BinaryOp.Ne =>
        if a.tpe != b.tpe then
          error(s"cannot compare a ${a.tpe.show} with a ${b.tpe.show} using '${op.symbol}'", pos)
          None
        else if a.tpe.isInstanceOf[Type.ListOf] then
          error(s"lists cannot be compared with '${op.symbol}'", pos)
          None
        else Some(Typed.Binary(op, a, b, Type.Bool))
      case cmp if cmp.isComparison =>
        if a.tpe == Type.Str && b.tpe == Type.Str then Some(Typed.Binary(op, a, b, Type.Bool))
        else both(Type.Num, Type.Bool)
      case _ => both(Type.Num, Type.Num)

object Typer:
  def forSchema(features: Map[String, FeatureType]): Typer =
    Typer(features.view.mapValues(Type.of).toMap)

  /** Closest candidate within an edit distance of 3, used for "did you mean" hints. */
  def suggest(name: String, candidates: Iterable[String]): Option[String] =
    candidates
      .map(c => c -> levenshtein(name, c))
      .filter((c, d) => d <= math.min(3, math.max(1, c.length / 3)))
      .toList
      .sortBy((c, d) => (d, c))
      .headOption
      .map(_._1)

  def levenshtein(a: String, b: String): Int =
    val prev = Array.tabulate(b.length + 1)(identity)
    val cur = Array.ofDim[Int](b.length + 1)
    for i <- 1 to a.length do
      cur(0) = i
      for j <- 1 to b.length do
        val cost = if a.charAt(i - 1) == b.charAt(j - 1) then 0 else 1
        cur(j) = math.min(math.min(cur(j - 1) + 1, prev(j) + 1), prev(j - 1) + cost)
      System.arraycopy(cur, 0, prev, 0, cur.length)
    prev(b.length)
