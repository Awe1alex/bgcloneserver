//package com.example.bgcloneserver
//
//import cats.effect.{ConcurrentEffect, Timer}
//import fs2.Stream
//import fs2.concurrent.{Queue, Topic}
//import org.http4s.client.blaze.BlazeClientBuilder
//import org.http4s.implicits._
//import org.http4s.server.blaze.BlazeServerBuilder
//import org.http4s.server.middleware.Logger
//
//import scala.concurrent.ExecutionContext.global
//
//object BgcloneServer {
//
//  def stream[F[_]: ConcurrentEffect](q: Queue[F, FromClient], t: Topic[F, ToClient])(implicit T: Timer[F]): Stream[F, Nothing] = {
//    for {
//      _ <- BlazeClientBuilder[F](global).stream
//
//      // Combine Service Routes into an HttpApp.
//      // Can also be done via a Router if you
//      // want to extract a segments not checked
//      // in the underlying routes.
//      httpApp = BgcloneRoutes.gameRoutes[F](q, t).orNotFound
//
//      // With Middlewares in place
//      finalHttpApp = Logger.httpApp(logHeaders = true, logBody = true)(httpApp)
//
//      exitCode <- BlazeServerBuilder[F](global)
//        .bindHttp(8080, "0.0.0.0")
//        .withHttpApp(finalHttpApp)
//        .serve
//    } yield exitCode
//  }.drain
//}
