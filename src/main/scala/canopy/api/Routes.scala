package canopy.api

import canopy.codec.JsonCodecs.given
import canopy.domain.Transaction
import canopy.engine.ScoringService
import canopy.features.{FeatureSchema, FeatureValue}
import canopy.ingest.{EventParser, IngestError, StreamOps}
import canopy.rules.{Diagnostic, RuleSet}
import cats.data.NonEmptyList
import cats.effect.IO
import fs2.Stream
import io.circe.syntax.*
import io.circe.{Json, ParsingFailure}
import org.http4s.*
import org.http4s.circe.*
import org.http4s.dsl.io.*
import org.http4s.headers.`Content-Type`

/**
 * HTTP surface of the engine.
 *
 * {{{
 * GET  /health                     liveness plus state counters
 * POST /v1/score[?dry_run=true]    score one transaction (JSON)
 * POST /v1/score/stream            score NDJSON transactions, respond with NDJSON decisions
 * POST /v1/features                feature vector a transaction would get, without recording it
 * POST /v1/labels                  report a fraud or legit outcome
 * GET  /v1/rules                   active rule program and compiled summary
 * PUT  /v1/rules                   replace the rule program (text body); 422 with diagnostics
 * POST /v1/rules/validate          compile a rule program without installing it
 * GET  /v1/model                   model metadata, thresholds and weights
 * GET  /v1/schema                  feature catalogue
 * GET  /metrics                    Prometheus text exposition
 * }}}
 */
final class Routes(service: ScoringService, maxBodyBytes: Long = 1L << 20):
  private object DryRun extends OptionalQueryParamDecoderMatcher[Boolean]("dry_run")

  private val ndjson = `Content-Type`(new MediaType("application", "x-ndjson"))

  private def problems(status: Status, messages: NonEmptyList[String]): IO[Response[IO]] =
    IO.pure(Response[IO](status).withEntity(Json.obj("errors" -> messages.toList.asJson)))

  private def readJson(req: Request[IO]): IO[Either[String, Json]] =
    req.body.take(maxBodyBytes + 1).compile.to(Array).map { bytes =>
      if bytes.length > maxBodyBytes then Left(s"request body exceeds $maxBodyBytes bytes")
      else
        io.circe.jawn.parseByteArray(bytes).left.map((e: ParsingFailure) =>
          s"invalid JSON: ${e.message}"
        )
    }

  private def withValid[A](req: Request[IO], decode: Json => EventParser.Result[A])(
      f: A => IO[Response[IO]]
  ): IO[Response[IO]] =
    readJson(req).flatMap {
      case Left(msg) =>
        service.metrics.recordRejected *> problems(Status.BadRequest, NonEmptyList.one(msg))
      case Right(json) =>
        decode(json).toEither match
          case Left(errs) =>
            service.metrics.recordRejected *> problems(Status.UnprocessableContent, errs)
          case Right(a) => f(a)
    }

  private def diagnosticsJson(source: String, ds: List[Diagnostic]): Json =
    Json.obj(
      "diagnostics" -> ds.map { d =>
        Json.obj(
          "severity" -> d.severity.toString.toLowerCase.asJson,
          "message" -> d.message.asJson,
          "line" -> d.pos.line.asJson,
          "column" -> d.pos.col.asJson,
          "rendered" -> d.render(source).asJson
        )
      }.asJson
    )

  private def rulesJson(rs: RuleSet): Json =
    Json.obj(
      "rules" -> rs.rules.map { r =>
        Json.obj(
          "name" -> r.name.asJson,
          "priority" -> r.priority.asJson,
          "action" -> r.action.render.asJson,
          "reason" -> r.reason.asJson,
          "tags" -> r.tags.asJson,
          "features" -> r.features.toList.sorted.asJson
        )
      }.asJson,
      "warnings" -> rs.warnings.map(_.summary).asJson
    )

  private def featureJson(v: FeatureValue): Json = v match
    case FeatureValue.Num(d)  => d.asJson
    case FeatureValue.Bool(b) => b.asJson
    case FeatureValue.Str(s)  => s.asJson

  private def scoreLine(line: String): IO[Json] =
    EventParser.parseLine(line, EventParser.transaction) match
      case Left(errs) => service.metrics.recordRejected.as(Json.obj("errors" -> errs.toList.asJson))
      case Right(tx)  => service.score(tx).map(_.asJson)

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "health" =>
      service.stats.flatMap { s =>
        Ok(
          Json.obj(
            "status" -> "ok".asJson,
            "processed" -> s.processed.asJson,
            "entities" -> s.entities.asJson,
            "graph_nodes" -> s.graphNodes.asJson,
            "graph_edges" -> s.graphEdges.asJson
          )
        )
      }

    case req @ POST -> Root / "v1" / "score" :? DryRun(dryRun) =>
      withValid(req, EventParser.transaction) { (tx: Transaction) =>
        val run = if dryRun.getOrElse(false) then service.preview(tx) else service.score(tx)
        run.flatMap(d => Ok(d.asJson))
      }

    case req @ POST -> Root / "v1" / "score" / "stream" =>
      val out: Stream[IO, Byte] = req.body
        .through(fs2.text.utf8.decode)
        .through(fs2.text.lines)
        .zipWithIndex
        .filter((line, _) => line.trim.nonEmpty)
        .evalMap { (line, idx) =>
          scoreLine(line).map(j =>
            j.deepMerge(Json.obj("line" -> (idx + 1).asJson)).noSpaces + "\n"
          )
        }
        .through(fs2.text.utf8.encode)
      IO.pure(Response[IO](Status.Ok).withEntity(out).withContentType(ndjson))

    case req @ POST -> Root / "v1" / "features" =>
      withValid(req, EventParser.transaction) { tx =>
        service.features(tx).flatMap { fv =>
          Ok(Json.fromFields(fv.values.toList.sortBy(_._1).map((k, v) => k -> featureJson(v))))
        }
      }

    case req @ POST -> Root / "v1" / "labels" =>
      withValid(req, EventParser.label)(l =>
        service.label(l) *> Accepted(Json.obj("status" -> "recorded".asJson))
      )

    case GET -> Root / "v1" / "rules" =>
      service.current.flatMap(c =>
        Ok(rulesJson(c.rules).deepMerge(Json.obj("source" -> c.rules.source.asJson)))
      )

    case req @ PUT -> Root / "v1" / "rules" =>
      req.as[String].flatMap { source =>
        service.replaceRules(source).flatMap {
          case Right(rs) => Ok(rulesJson(rs))
          case Left(ds)  => UnprocessableContent(diagnosticsJson(source, ds))
        }
      }

    case req @ POST -> Root / "v1" / "rules" / "validate" =>
      req.as[String].flatMap { source =>
        service.validateRules(source).flatMap {
          case Right(rs) => Ok(rulesJson(rs))
          case Left(ds)  => UnprocessableContent(diagnosticsJson(source, ds))
        }
      }

    case GET -> Root / "v1" / "model" =>
      service.current.flatMap { c =>
        val m = c.model
        Ok(
          Json.obj(
            "version" -> m.version.asJson,
            "intercept" -> m.intercept.asJson,
            "thresholds" -> m.thresholds.asJson,
            "validation" -> m.validation.asJson,
            "weights" -> m.weights.map((k, w) =>
              Json.obj("column" -> k.asJson, "weight" -> w.asJson)
            ).asJson
          )
        )
      }

    case GET -> Root / "v1" / "schema" =>
      Ok(FeatureSchema.all.map { s =>
        Json.obj(
          "name" -> s.name.asJson,
          "type" -> s.tpe.toString.toLowerCase.asJson,
          "description" -> s.description.asJson
        )
      }.asJson)

    case GET -> Root / "metrics" =>
      for
        stats <- service.stats
        text <- service.metrics.render(
          List(
            (
              "canopy_tracked_entities",
              "Entities with live feature state.",
              stats.entities.toDouble
            ),
            ("canopy_graph_nodes", "Nodes in the entity graph.", stats.graphNodes.toDouble),
            ("canopy_graph_edges", "Edges in the entity graph.", stats.graphEdges.toDouble)
          )
        )
        resp <- Ok(text)
      yield resp.withContentType(`Content-Type`(MediaType.text.plain))
  }

object Routes:
  /** Shared NDJSON decoder for CLI file inputs, kept next to the streaming endpoint it mirrors. */
  def decodeTransactions(bytes: Stream[IO, Byte]): Stream[IO, Either[IngestError, Transaction]] =
    bytes.through(StreamOps.ndjson(line =>
      EventParser.parseLine(line, EventParser.transaction).left.map(_.toList)
    ))
