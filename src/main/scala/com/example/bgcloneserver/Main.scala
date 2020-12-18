package com.example.bgcloneserver

import com.example.bgcloneserver.CommandADT._
import io.circe.parser.decode
import cats.effect._
import cats.syntax.all._
import fs2._
import fs2.concurrent.Queue
import org.http4s._
import org.http4s.dsl.Http4sDsl
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.websocket._
import org.http4s.websocket.WebSocketFrame
import org.http4s.websocket.WebSocketFrame._

import scala.concurrent.ExecutionContext.global
import scala.concurrent.duration._


object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    BattleGroundsApp[IO].stream.compile.drain.as(ExitCode.Success)
}

class BattleGroundsApp[F[_]](implicit F: ConcurrentEffect[F], timer: Timer[F])
  extends Http4sDsl[F] {
  def routes: HttpRoutes[F] =
    HttpRoutes.of[F] {
      case GET -> Root / "ws" =>
        val toClient: Stream[F, WebSocketFrame] =
          Stream.awakeEvery[F](1.seconds).map(d => Text(s"Ping! $d"))
        val fromClient: Pipe[F, WebSocketFrame, Unit] = _.evalMap {
          case Text(t, _) => F.delay(processCommand(decode[Command](t)))
          case f => F.delay(println(s"Unknown type: $f"))
        }
        WebSocketBuilder[F].build(toClient, fromClient)
    }

  def stream: Stream[F, ExitCode] =
    BlazeServerBuilder[F](global)
      .bindHttp(8080)
      .withHttpApp(routes.orNotFound)
      .serve
}

object BattleGroundsApp {
  def apply[F[_]: ConcurrentEffect: Timer]: BattleGroundsApp[F] =
    new BattleGroundsApp[F]
}

