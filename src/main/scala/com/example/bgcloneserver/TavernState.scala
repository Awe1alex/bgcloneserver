package com.example.bgcloneserver

import cats.effect.IO
import cats.effect.concurrent.Ref
import com.example.bgcloneserver.Tavern._

object TavernState {
  // Tavern state ADTs, tavern state exists for each player individually
  // 1. Coins
  // 3. Level
  // 4. Available cards
  // 5. Rolled Cards
  // 6. Frozen Cards
  // 7. Cards in hand


  final class PlayerStates(ref: Ref[IO, PlayerState]) {

    def nextTurn: IO[PlayerState] =
      for {
        updatedState <- ref.modify(oldState => {
          val baseCoins = if (oldState.baseCoins == 10) 10 else oldState.baseCoins + 1
          val upgradeCost = if (oldState.upgradeCost == 0) 0 else oldState.upgradeCost - 1
          val rolledCards = oldState.rollCards
          val newState = oldState.copy(baseCoins = baseCoins, coins = baseCoins, upgradeCost = upgradeCost, rolledCards = rolledCards)
          (newState, newState)
        })
    } yield updatedState


    def getCoins: IO[Int] =
      ref.get.map(_.coins)
  }

  case class PlayerState(name: String,
                         baseCoins: Int = 3,
                         coins: Int = 3,
                         level: Int = 1,
                         upgradeCost: Int = 5,
                         availableCards: List[Card] = Tavern.AllCardsByLevel.head,
                         rolledCards: List[Card] = List(),
                         frozenCards: List[Card] = List(),
                         cardsInHand: List[Card] = List()) {
    def rollCards: List[Card] = availableCards
  }

  val example: IO[Unit] = for {
    ref <- Ref[IO].of(PlayerState(name = "Awe1"))
    playerStates = new PlayerStates(ref)
    playerState <- playerStates.nextTurn
  } yield println(playerState)








//  final case class Coins private (value: Int) extends AnyVal
//  object Coins {
//    def create(value: Int): Option[Coins] = {
//      if (value < 0) None
//      else if (value > 10) None
//      else Option(Coins(value))
//    }
//
//    def update(value: Int): Option[Coins] = {
//      if (value < 0) None
//      else if (value > 10) Option(Coins(10))
//      else Option(Coins(value))
//    }
//  }
//
//  final case class Level private (value: Int) extends AnyVal
//  object Level {
//    def create(value: Int): Option[Level] = {
//      if (value < 1) None
//      else if (value > 6) None
//      else Option(Level(value))
//    }
//  }
//
//  final case class AvailableCards()
}
