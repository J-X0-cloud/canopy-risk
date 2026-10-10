package canopy.rules

import canopy.features.{FeatureSchema, FeatureVector}

/** A rule whose condition held for a given feature vector. */
final case class Firing(
    rule: String,
    priority: Int,
    action: Action,
    reason: String,
    tags: List[String]
)

final case class CompiledRule(
    name: String,
    priority: Int,
    action: Action,
    reason: String,
    tags: List[String],
    features: Set[String],
    condition: Evaluator.Compiled
)

/**
 * A type-checked, compiled rule program ready for evaluation. Rules are kept in descending priority
 * order; ties keep their source order so authors control precedence explicitly.
 */
final case class RuleSet(source: String, rules: Vector[CompiledRule], warnings: List[Diagnostic]):
  def evaluate(fv: FeatureVector): List[Firing] =
    rules.iterator
      .filter(r => r.condition(fv) == Value.VBool(true))
      .map(r => Firing(r.name, r.priority, r.action, r.reason, r.tags))
      .toList

  /** Evaluates every rule and reports its raw three-valued outcome, for debugging tools. */
  def trace(fv: FeatureVector): List[(String, Value)] =
    rules.toList.map(r => r.name -> r.condition(fv))

  def size: Int = rules.size

object RuleSet:
  val empty: RuleSet = RuleSet("", Vector.empty, Nil)

  private lazy val defaultTyper = Typer.forSchema(FeatureSchema.types)

  /** Parses, type checks and compiles. Errors are returned with every diagnostic found. */
  def compile(source: String, typer: Typer = defaultTyper): Either[List[Diagnostic], RuleSet] =
    for
      program <- Parser.parse(source).left.map(List(_))
      checked <- typer.check(program)
    yield
      val (typed, warnings) = checked
      val compiled = typed.rules.zipWithIndex
        .sortBy((tr, idx) => (-tr.rule.priority, idx))
        .map { (tr, _) =>
          val r = tr.rule
          CompiledRule(
            r.name,
            r.priority,
            r.action,
            r.reason.getOrElse(r.name),
            r.tags,
            tr.condition.features,
            Evaluator.compile(tr.condition)
          )
        }
      RuleSet(source, compiled.toVector, warnings)

  def renderErrors(source: String, diagnostics: List[Diagnostic]): String =
    diagnostics.map(_.render(source)).mkString("\n\n")
