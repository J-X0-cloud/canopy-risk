package canopy.cli

import canopy.codec.JsonCodecs.given
import canopy.domain.{Label, Transaction}
import canopy.ingest.{EventParser, IngestError, StreamOps}
import canopy.ml.LogisticModel
import canopy.rules.RuleSet
import cats.effect.IO
import fs2.Stream
import fs2.io.file.{Files, Path}
import io.circe.syntax.*

/** File helpers shared by the CLI commands. */
object Io:
  val defaultModelResource = "default-model.json"
  val defaultRulesResource = "default.rules"

  def readText(path: String): IO[String] = Files[IO].readUtf8(Path(path)).compile.string

  def writeText(path: String, text: String): IO[Unit] =
    val p = Path(path)
    p.parent.fold(IO.unit)(Files[IO].createDirectories(_)) *>
      Stream.emit(text).through(Files[IO].writeUtf8(p)).compile.drain

  def resource(name: String): IO[String] =
    IO.blocking(Option(getClass.getClassLoader.getResourceAsStream(name))).flatMap {
      case None => IO.raiseError(IllegalStateException(s"bundled resource $name is missing"))
      case Some(in) =>
        IO.blocking(try String(in.readAllBytes(), "UTF-8") finally in.close())
    }

  def parseModel(text: String, origin: String): IO[LogisticModel] =
    IO.fromEither(
      io.circe.parser.decode[LogisticModel](text).left.map(e => IllegalArgumentException(s"$origin: ${e.getMessage}"))
    )

  def loadModel(path: Option[String]): IO[LogisticModel] = path match
    case Some(p) => readText(p).flatMap(parseModel(_, p))
    case None    => resource(defaultModelResource).flatMap(parseModel(_, s"resource $defaultModelResource"))

  def loadRules(path: Option[String]): IO[RuleSet] =
    path.fold(resource(defaultRulesResource))(readText).flatMap { source =>
      RuleSet.compile(source) match
        case Right(rs) => IO.pure(rs)
        case Left(ds) =>
          IO.raiseError(IllegalArgumentException("rules failed to compile:\n" + RuleSet.renderErrors(source, ds)))
    }

  def saveModel(path: String, model: LogisticModel): IO[Unit] = writeText(path, model.asJson.spaces2 + "\n")

  def transactions(path: String): Stream[IO, Either[IngestError, Transaction]] =
    Files[IO].readAll(Path(path)).through(
      StreamOps.ndjson(line => EventParser.parseLine(line, EventParser.transaction).left.map(_.toList))
    )

  def labels(path: String): Stream[IO, Either[IngestError, Label]] =
    Files[IO].readAll(Path(path)).through(
      StreamOps.ndjson(line => EventParser.parseLine(line, EventParser.label).left.map(_.toList))
    )

  /** Reads a file strictly, failing on the first malformed line with its line number. */
  def strict[A](s: Stream[IO, Either[IngestError, A]], path: String): IO[Vector[A]] =
    s.evalMap {
      case Right(a) => IO.pure(a)
      case Left(e)  => IO.raiseError(IllegalArgumentException(s"$path ${e.render}"))
    }.compile.toVector

  def writeNdjson[A: io.circe.Encoder](path: String, items: Seq[A]): IO[Unit] =
    val p = Path(path)
    p.parent.fold(IO.unit)(Files[IO].createDirectories(_)) *>
      Stream.emits(items).map(_.asJson.noSpaces + "\n").through(Files[IO].writeUtf8(p)).compile.drain
