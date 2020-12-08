package com.example.bgcloneserver

import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp}
import fs2.concurrent.{Queue, Topic}
import fs2.Stream

case class State(messageCount: Int)

case class FromClient(username: String, message: String)
case class ToClient(message: String)

object Main extends IOApp {
  def run(args: List[String]) = {
    for (
      q <- Queue.unbounded[IO, FromClient];
      t <- Topic[IO, ToClient](ToClient("Connected"));
      ref <- Ref.of[IO, State](State(1));
      exitCode <- {
        val commandsStream = q
          .dequeue
          .evalMap(fromClient => {
            ref.modify(currentState => {
              ( State(currentState.messageCount + 1)
                , ToClient(s"${currentState.messageCount}: ${fromClient.username} - ${fromClient.message}"))
            })
          })
          .through(t.publish)

        val serverStream = BgcloneServer.stream[IO](q, t)
        val combinedStreams = Stream(commandsStream, serverStream).parJoinUnbounded

        combinedStreams.compile.drain.as(ExitCode.Success)
      }
    ) yield exitCode
  }
}
