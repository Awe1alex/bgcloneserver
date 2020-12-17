package com.example.bgcloneserver

import cats.effect.IO
import cats.effect.concurrent.Ref
import com.example.bgcloneserver.TavernState.{PlayerState, PlayerStates}

object Player {
  // Player ADTs to implement:
  // 1. Nickname
  // 2. HP
  // 3. Cards on board
  // 5. Tavern State (incl cards in hand)
  // 6. Hero (in future)

  val example: IO[Unit] = for {
    ref <- Ref[IO].of(PlayerState(name = "Awe1"))
    playerStates = new PlayerStates(ref)
    playerState <- playerStates.nextTurn
  } yield println(playerState.rolledCards)
}
