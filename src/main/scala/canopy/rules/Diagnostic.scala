package canopy.rules

enum Severity:
  case Error, Warning

/** A compiler message tied to a source position. */
final case class Diagnostic(severity: Severity, message: String, pos: Pos):
  def isError: Boolean = severity == Severity.Error

  def summary: String = s"${severity.toString.toLowerCase} at $pos: $message"

  /** Renders the message with the offending source line and a caret under the column. */
  def render(source: String): String =
    val lines = source.linesIterator.toVector
    val line = lines.lift(pos.line - 1).getOrElse("")
    val gutter = pos.line.toString
    val pad = " " * gutter.length
    s"""$summary
       |$pad |
       |$gutter | $line
       |$pad | ${" " * math.max(0, pos.col - 1)}^""".stripMargin
