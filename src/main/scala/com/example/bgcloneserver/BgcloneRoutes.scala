package com.example.bgcloneserver

import cats.effect.Sync
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import fs2.concurrent.{Queue, Topic}
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame


object BgcloneRoutes {

//  def gameRoutes[F[_]: Sync](q: Queue[F, FromClient], t: Topic[F, ToClient], ref: Ref[F, State]): HttpRoutes[F] = {
  def gameRoutes[F[_]: Sync](q: Queue[F, FromClient], t: Topic[F, ToClient]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F]{}
    import dsl._

    HttpRoutes.of[F] {
      case GET -> Root / "ws" / userName =>
        val toClient = t
          .subscribe(1000)
          .collect(toClientMessage => {
            WebSocketFrame.Text(toClientMessage.message)
          })

        WebSocketBuilder[F].build(toClient, _.collect({
          case WebSocketFrame.Text(text, _) =>
            FromClient(userName, text)
          }).through(q.enqueue))
    }
  }
}