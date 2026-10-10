package canopy.features

/** A single feature value. Absence of a key in a [[FeatureVector]] means "missing". */
enum FeatureValue:
  case Num(value: Double)
  case Bool(value: Boolean)
  case Str(value: String)

  /** Numeric view used by the model: booleans become 0/1, strings have no numeric meaning. */
  def asDouble: Option[Double] = this match
    case Num(v)  => Some(v)
    case Bool(b) => Some(if b then 1.0 else 0.0)
    case Str(_)  => None

  def render: String = this match
    case Num(v)  => if v.isWhole && math.abs(v) < 1e15 then v.toLong.toString else v.toString
    case Bool(b) => b.toString
    case Str(s)  => s

/** The declared type of a feature, used by the rules type checker. */
enum FeatureType:
  case Number, Boolean, Text

final case class FeatureVector(values: Map[String, FeatureValue]):
  def get(name: String): Option[FeatureValue] = values.get(name)

  def num(name: String): Option[Double] = values.get(name).flatMap(_.asDouble)

  def +(kv: (String, FeatureValue)): FeatureVector = FeatureVector(values + kv)

  def ++(other: FeatureVector): FeatureVector = FeatureVector(values ++ other.values)

  def withNum(name: String, v: Double): FeatureVector =
    if v.isNaN || v.isInfinite then FeatureVector(values - name)
    else FeatureVector(values.updated(name, FeatureValue.Num(v)))

  def withBool(name: String, v: Boolean): FeatureVector =
    FeatureVector(values.updated(name, FeatureValue.Bool(v)))

  def withStr(name: String, v: String): FeatureVector =
    FeatureVector(values.updated(name, FeatureValue.Str(v)))

  /** Applies `f` only when the optional input is present. */
  def withSome[A](opt: Option[A])(f: (FeatureVector, A) => FeatureVector): FeatureVector =
    opt.fold(this)(f(this, _))

object FeatureVector:
  val empty: FeatureVector = FeatureVector(Map.empty)
