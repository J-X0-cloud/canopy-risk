package canopy.api

import canopy.TestData.*
import canopy.cli.Io
import canopy.codec.JsonCodecs.given
import canopy.engine.ScoringService
import cats.effect.{IO, Resource}
import io.circe.Json
import io.circe.syntax.*
import munit.CatsEffectSuite
import org.http4s.*
import org.http4s.circe.*
import org.http4s.implicits.*

class RoutesSuite extends CatsEffectSuite:
  private val app = ResourceFunFixture(
    Resource.eval(
      for
        model <- Io.loadModel(None)
        rules <- Io.loadRules(None)
        s <- ScoringService.create(model, rules)
      yield Server.app(s)
    )
  )

  private def post(uri: Uri, body: Json): Request[IO] =
    Request[IO](Method.POST, uri).withEntity(body)

  app.test("health reports ok") { http =>
    http.run(Request[IO](Method.GET, uri"/health")).flatMap { r =>
      assertEquals(r.status, Status.Ok)
      r.as[Json].map(j => assertEquals(j.hcursor.get[String]("status"), Right("ok")))
    }
  }

  app.test("score returns a decision with an explanation") { http =>
    http.run(post(uri"/v1/score", tx("api-1").asJson)).flatMap { r =>
      assertEquals(r.status, Status.Ok)
      r.as[Json].map { j =>
        val c = j.hcursor
        assertEquals(c.get[String]("transaction_id"), Right("api-1"))
        assert(c.get[String]("verdict").isRight)
        assert(c.downField("explanation").get[String]("summary").isRight)
      }
    }
  }

  app.test("validation errors are 422 with every message, malformed JSON is 400") { http =>
    for
      bad <- http.run(post(uri"/v1/score", Json.obj("id" -> "x".asJson)))
      body <- bad.as[Json]
      broken <- http.run(Request[IO](Method.POST, uri"/v1/score").withEntity("{not json"))
    yield
      assertEquals(bad.status, Status.UnprocessableContent)
      assert(body.hcursor.get[List[String]]("errors").exists(_.size >= 5), body.noSpaces)
      assertEquals(broken.status, Status.BadRequest)
  }

  app.test("dry run does not record the transaction") { http =>
    for
      _ <- http.run(post(uri"/v1/score?dry_run=true", tx("dry").asJson))
      h <- http.run(Request[IO](Method.GET, uri"/health")).flatMap(_.as[Json])
    yield assertEquals(h.hcursor.get[Long]("processed"), Right(0L))
  }

  app.test("streaming endpoint answers NDJSON line by line, including errors") { http =>
    val body = List(
      tx("s1").asJson.noSpaces,
      "",
      "{\"oops\": 1}",
      tx("s2", at = t0.plusMinutes(1)).asJson.noSpaces
    ).mkString("\n")
    http.run(Request[IO](Method.POST, uri"/v1/score/stream").withEntity(body)).flatMap { r =>
      r.as[String].map { text =>
        val lines = text.linesIterator.toList.map(io.circe.parser.parse(_).toOption.get.hcursor)
        assertEquals(lines.map(_.get[Long]("line").toOption.get), List(1L, 3L, 4L))
        assert(lines(1).downField("errors").succeeded)
        assertEquals(lines(2).get[String]("transaction_id"), Right("s2"))
      }
    }
  }

  app.test("rules can be read, validated and replaced; bad rules give located diagnostics") {
    http =>
      for
        current <- http.run(Request[IO](Method.GET, uri"/v1/rules")).flatMap(_.as[Json])
        invalid <- http.run(Request[IO](
          Method.PUT,
          uri"/v1/rules"
        ).withEntity("rule r\n  when amout > 1\n  then approve"))
        diag <- invalid.as[Json]
        ok <- http.run(Request[IO](
          Method.POST,
          uri"/v1/rules/validate"
        ).withEntity("rule r when amount > 1 then approve"))
      yield
        assert(current.hcursor.downField("rules").values.exists(_.nonEmpty))
        assertEquals(invalid.status, Status.UnprocessableContent)
        val d = diag.hcursor.downField("diagnostics").downArray
        assertEquals(d.get[Int]("line"), Right(2))
        assertEquals(d.get[Int]("column"), Right(8))
        assertEquals(ok.status, Status.Ok)
  }

  app.test("labels, model, schema, features and metrics endpoints") { http =>
    val label = Json.obj(
      "transaction_id" -> "t".asJson,
      "card_id" -> "c".asJson,
      "outcome" -> "fraud".asJson,
      "reported_at" -> t0.toString.asJson
    )
    for
      l <- http.run(post(uri"/v1/labels", label))
      model <- http.run(Request[IO](Method.GET, uri"/v1/model")).flatMap(_.as[Json])
      schema <- http.run(Request[IO](Method.GET, uri"/v1/schema")).flatMap(_.as[Json])
      features <- http.run(post(uri"/v1/features", tx("f").asJson)).flatMap(_.as[Json])
      metrics <- http.run(Request[IO](Method.GET, uri"/metrics")).flatMap(_.as[String])
      missing <- http.run(Request[IO](Method.GET, uri"/nope"))
    yield
      assertEquals(l.status, Status.Accepted)
      assert(model.hcursor.downField("weights").values.exists(_.nonEmpty))
      assert(schema.asArray.exists(_.size > 30))
      assertEquals(features.hcursor.get[Double]("card.count_1h"), Right(1.0))
      assert(metrics.contains("canopy_labels_total 1"), metrics)
      assertEquals(missing.status, Status.NotFound)
  }

  app.test("oversized bodies are rejected") { http =>
    val huge = Json.obj("pad" -> ("x" * (2 << 20)).asJson)
    http.run(post(uri"/v1/score", huge)).map(r => assertEquals(r.status, Status.BadRequest))
  }
