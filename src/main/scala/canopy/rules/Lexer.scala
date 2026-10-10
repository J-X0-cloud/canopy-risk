package canopy.rules

import scala.annotation.tailrec

enum Tok:
  case Ident(name: String)
  case Num(value: Double, text: String)
  case Str(value: String)
  case Kw(word: String)
  case Sym(symbol: String)
  case Eof

  def describe: String = this match
    case Ident(n)  => s"identifier '$n'"
    case Num(_, t) => s"number $t"
    case Str(s)    => s"string \"$s\""
    case Kw(w)     => s"'$w'"
    case Sym(s)    => s"'$s'"
    case Eof       => "end of input"

final case class Token(tok: Tok, pos: Pos)

/**
 * Hand-written scanner for the rules language.
 *
 * Identifiers may contain dots (`card.count_1h`) so feature paths are single tokens. Numbers may
 * use `_` as a digit separator. `#` starts a comment that runs to the end of the line.
 */
object Lexer:
  val keywords: Set[String] = Set(
    "rule",
    "let",
    "when",
    "then",
    "reason",
    "priority",
    "tag",
    "and",
    "or",
    "not",
    "in",
    "between",
    "true",
    "false",
    "approve",
    "review",
    "decline",
    "score"
  )

  private val twoCharSymbols = Set("==", "!=", "<=", ">=")
  private val oneCharSymbols = Set('(', ')', '[', ']', ',', '+', '-', '*', '/', '%', '<', '>', '=')

  def tokenize(src: String): Either[Diagnostic, Vector[Token]] =
    val n = src.length

    def err(msg: String, line: Int, col: Int) =
      Left(Diagnostic(Severity.Error, msg, Pos(line, col)))

    def isIdentStart(c: Char) = c.isLetter || c == '_'
    def isIdentPart(c: Char) = c.isLetterOrDigit || c == '_'

    @tailrec
    def loop(i: Int, line: Int, col: Int, acc: Vector[Token]): Either[Diagnostic, Vector[Token]] =
      if i >= n then Right(acc :+ Token(Tok.Eof, Pos(line, col)))
      else
        val c = src.charAt(i)
        val here = Pos(line, col)
        if c == '\n' then loop(i + 1, line + 1, 1, acc)
        else if c.isWhitespace then loop(i + 1, line, col + 1, acc)
        else if c == '#' then
          val end = src.indexOf('\n', i)
          val stop = if end < 0 then n else end
          loop(stop, line, col + (stop - i), acc)
        else if isIdentStart(c) then
          scanIdent(src, i) match
            case Left(badAt) =>
              err("identifier segment expected after '.'", line, col + (badAt - i))
            case Right(end) =>
              val word = src.substring(i, end)
              val tok = if keywords.contains(word) then Tok.Kw(word) else Tok.Ident(word)
              loop(end, line, col + (end - i), acc :+ Token(tok, here))
        else if c.isDigit then
          scanNumber(src, i) match
            case Left((msg, at)) => err(msg, line, col + (at - i))
            case Right((end, value)) =>
              val text = src.substring(i, end)
              loop(end, line, col + (end - i), acc :+ Token(Tok.Num(value, text), here))
        else if c == '"' then
          scanString(src, i + 1) match
            case Left((msg, at)) => err(msg, line, col + (at - i))
            case Right((end, value)) =>
              loop(end, line, col + (end - i), acc :+ Token(Tok.Str(value), here))
        else
          val two = if i + 1 < n then src.substring(i, i + 2) else ""
          if twoCharSymbols.contains(two) then
            loop(i + 2, line, col + 2, acc :+ Token(Tok.Sym(two), here))
          else if oneCharSymbols.contains(c) then
            loop(i + 1, line, col + 1, acc :+ Token(Tok.Sym(c.toString), here))
          else err(s"unexpected character '$c'", line, col)

    def scanIdent(s: String, start: Int): Either[Int, Int] =
      @tailrec def segment(i: Int): Either[Int, Int] =
        var j = i
        while j < n && isIdentPart(s.charAt(j)) do j += 1
        if j < n && s.charAt(j) == '.' then
          if j + 1 < n && isIdentStart(s.charAt(j + 1)) then segment(j + 1) else Left(j + 1)
        else Right(j)
      segment(start)

    def scanNumber(s: String, start: Int): Either[(String, Int), (Int, Double)] =
      var j = start
      def digits(): Unit =
        while j < n && (s.charAt(j).isDigit || s.charAt(j) == '_') do j += 1
      digits()
      if j < n && s.charAt(j) == '.' && j + 1 < n && s.charAt(j + 1).isDigit then
        j += 1
        digits()
      if j < n && (s.charAt(j) == 'e' || s.charAt(j) == 'E') then
        val save = j
        j += 1
        if j < n && (s.charAt(j) == '+' || s.charAt(j) == '-') then j += 1
        if j < n && s.charAt(j).isDigit then digits() else j = save
      val raw = s.substring(start, j)
      if j < n && isIdentStart(s.charAt(j)) then
        Left((s"invalid character '${s.charAt(j)}' in number", j))
      else if raw.endsWith("_") || raw.contains("__") || raw.contains("_.") || raw.contains("._")
      then
        Left((s"misplaced digit separator in '$raw'", start))
      else Right((j, raw.replace("_", "").toDouble))

    def scanString(s: String, start: Int): Either[(String, Int), (Int, String)] =
      val sb = StringBuilder()
      @tailrec def go(i: Int): Either[(String, Int), (Int, String)] =
        if i >= n || s.charAt(i) == '\n' then Left(("unterminated string literal", start - 1))
        else
          s.charAt(i) match
            case '"' => Right((i + 1, sb.result()))
            case '\\' if i + 1 < n =>
              s.charAt(i + 1) match
                case 'n'   => sb.append('\n'); go(i + 2)
                case 't'   => sb.append('\t'); go(i + 2)
                case '"'   => sb.append('"'); go(i + 2)
                case '\\'  => sb.append('\\'); go(i + 2)
                case other => Left((s"unknown escape '\\$other'", i))
            case ch => sb.append(ch); go(i + 1)
      go(start)

    loop(0, 1, 1, Vector.empty)
