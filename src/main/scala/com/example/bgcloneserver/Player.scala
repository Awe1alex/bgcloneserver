package com.example.bgcloneserver


import cats.effect.IO
import cats.effect.concurrent.Ref
import com.example.bgcloneserver.Tavern._
import com.example.bgcloneserver.TavernState._

object Player {
  // Player ADTs to implement:
  // 1. Nickname
  // 2. HP
  // 3. Cards on board
  // 5. Tavern State (incl cards in hand)
  // 6. Hero (in future)

   def newPlayer(name: String): IO[PlayerStates] = for {
    ref <- Ref[IO].of(PlayerState(name = name))
    playerStates = new PlayerStates(ref)
  } yield playerStates
}
