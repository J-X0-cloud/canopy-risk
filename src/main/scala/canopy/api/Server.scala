package canopy.api

import canopy.engine.ScoringService
import cats.effect.{IO, Resource}
import com.comcast.ip4s.{Host, Port}
import org.http4s.HttpApp
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.Server as Http4sServer
import org.http4s.server.middleware.{ErrorAction, ErrorHandling}

import scala.concurrent.duration.*

object Server:
  def app(service: ScoringService): HttpApp[IO] =
    val base = Routes(service).routes.orNotFound
    ErrorHandling.Recover.total(
      ErrorAction.log(
        base,
        messageFailureLogAction = (t, msg) => IO.println(s"[warn] $msg: ${t.getMessage}"),
        serviceErrorLogAction = (t, msg) => IO.println(s"[error] $msg: ${t.getMessage}")
      )
    )

  def resource(service: ScoringService, host: String, port: Int): Resource[IO, Http4sServer] =
    for
      h <- Resource.eval(IO.fromOption(Host.fromString(host))(IllegalArgumentException(s"bad host $host")))
      p <- Resource.eval(IO.fromOption(Port.fromInt(port))(IllegalArgumentException(s"bad port $port")))
      server <- EmberServerBuilder
        .default[IO]
        .withHost(h)
        .withPort(p)
        .withHttpApp(app(service))
        .withShutdownTimeout(5.seconds)
        .build
    yield server
