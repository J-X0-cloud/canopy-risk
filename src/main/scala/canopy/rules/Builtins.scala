package canopy.rules

/** Built-in functions available in rule expressions. */
object Builtins:
  enum Fn(val name: String):
    case Abs extends Fn("abs")
    case Min extends Fn("min")
    case Max extends Fn("max")
    case Log1p extends Fn("log1p")
    case Sqrt extends Fn("sqrt")
    case Missing extends Fn("missing")
    case Present extends Fn("present")
    case Coalesce extends Fn("coalesce")
    case Lower extends Fn("lower")
    case StartsWith extends Fn("starts_with")
    case Contains extends Fn("contains")

    /** Checks argument types and returns the result type, or a message describing the mismatch. */
    def resultType(args: List[Type]): Either[String, Type] =
      import Type.*
      def arity(n: Int) =
        if args.size == n then Right(()) else Left(s"expects $n argument(s), got ${args.size}")
      this match
        case Abs | Log1p | Sqrt =>
          arity(1).flatMap(_ =>
            if args.head == Num then Right(Num)
            else Left(s"expects a number, got ${args.head.show}")
          )
        case Min | Max =>
          if args.size < 2 then Left(s"expects at least 2 arguments, got ${args.size}")
          else if args.forall(_ == Num) then Right(Num)
          else Left("expects only numbers")
        case Missing | Present => arity(1).map(_ => Bool)
        case Coalesce =>
          if args.size < 2 then Left(s"expects at least 2 arguments, got ${args.size}")
          else if args.forall(_ == args.head) then Right(args.head)
          else
            Left(s"all arguments must have the same type, got ${args.map(_.show).mkString(", ")}")
        case Lower =>
          arity(1).flatMap(_ =>
            if args.head == Str then Right(Str)
            else Left(s"expects a string, got ${args.head.show}")
          )
        case StartsWith | Contains =>
          arity(2).flatMap(_ =>
            if args.forall(_ == Str) then Right(Bool)
            else Left(s"expects two strings, got ${args.map(_.show).mkString(", ")}")
          )

  private val byName: Map[String, Fn] = Fn.values.map(f => f.name -> f).toMap

  def lookup(name: String): Option[Fn] = byName.get(name)

  def names: Set[String] = byName.keySet
