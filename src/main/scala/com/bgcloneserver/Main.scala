package com.bgcloneserver

import io.circe.parser._
import cats.effect._
import cats.syntax.all._
import com.bgcloneserver.CommandADT.Command
import GameState.PlayerStates
import CommandADT.CommandJson.commandDecoder
import cats.effect.concurrent.Ref
import com.bgcloneserver.PlayerADTS.Player
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

//    val p1 = new BattleGroundsApp[IO]
//    val p2 = new BattleGroundsApp[IO]
//  val stream: Stream[Pure, INothing] = Stream(p1, p2).parJoin(2).drain
//  stream.compile.drain.as(ExitCode.Success)
    BattleGroundsApp[IO].stream.compile.drain.as(ExitCode.Success)
}

class BattleGroundsApp[F[_]](implicit F: ConcurrentEffect[F], timer: Timer[F])
  extends Http4sDsl[F] {

  def routes: HttpRoutes[F] = {
    val players = for {
      ref <- Ref[IO].of(Map.empty[String, Player])
      playerStates = new PlayerStates(ref)
      _ <- playerStates.addPlayer(Player(name = "Awe1"))
      _ <- playerStates.addPlayer(Player(name = "Awe2"))
      _ <- playerStates.nextTurn
    } yield playerStates

    players.unsafeRunSync().buyCard("Awe1", 1).unsafeRunSync()

    HttpRoutes.of[F] {
      case GET -> Root / "ws" =>
        val toClient: Stream[F, WebSocketFrame] =
          Stream.awakeEvery[F](1.seconds).map(d => Text(s"Ping! $d"))
        val fromClient: Pipe[F, WebSocketFrame, Unit] = _.evalMap {
          case Text(t, _) =>
            val command = for {
              parsedJson <- parse(t)
              parsedCommand <- parsedJson.as[Command]
            } yield parsedCommand
            command match {
              case Right(x) => F.delay(println(players.unsafeRunSync().processCommand(x)))
              case Left(x) => F.delay(println(x))
            }
            case f => F.delay(println(s"Unknown type: $f"))
        }
        WebSocketBuilder[F].build(toClient, fromClient)

      case GET -> Root / "wsecho" =>
        val echoReply: Pipe[F, WebSocketFrame, WebSocketFrame] =
          _.collect {
            case Text(msg, _) => Text("You sent the server: " + msg)
            case _ => Text("Something new")
          }

        for {
          q <- Queue.unbounded[F, WebSocketFrame]
          response <- WebSocketBuilder[F].build(q.dequeue.through(echoReply), q.enqueue)
        } yield response
    }
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
//  def run(args: List[String]): IO[ExitCode] = {
//
//    TavernState.example.unsafeRunSync()
//
//    for (
//      q <- Queue.unbounded[IO, FromClient];
//      t <- Topic[IO, ToClient](ToClient("Connected"));
//      ref <- Ref[IO].of(State(1));
//      exitCode <- {
//        // Commands from client stream, here I should implement calculations
//        val commandsStream = q
//          .dequeue
//          .evalMap(fromClient => {
//            ref.modify(currentState => {
//              ( State(currentState.messageCount + 1)
//                , ToClient(s"${currentState.messageCount}: ${fromClient.username} - ${fromClient.message}"))
//            })
//          })
//          .through(t.publish)
//
//        val serverStream = BgcloneServer.stream[IO](q, t)
//        val combinedStreams = Stream(commandsStream, serverStream).parJoinUnbounded
//
//        combinedStreams.compile.drain.as(ExitCode.Success)
//      }
//    ) yield exitCode
//  }
//}
