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

    def upgradeTavern: IO[PlayerState] =
      for {
        updatedState <- ref.modify(oldState => {
          if (oldState.level.value <= 6) {
            oldState.coins.buy(oldState.upgradeCost) match {
              case Right(x) =>
                val coins = x
                val level = oldState.level.increase
                val upgradeCost = level.nextUpgradeBaseCost
                val availableCards = oldState.updatePool(oldState.availableCards, level)
                val newState = oldState.copy(coins = coins, level = level, upgradeCost = upgradeCost, availableCards = availableCards)
                (newState, newState)
              case Left(_) =>
                (oldState, oldState)
            }
          } else {
            (oldState, oldState)
          }
        })
      } yield updatedState

    def buyCard(cardId: Int): IO[PlayerState] = {
      //Move card from rolled cards to hand
      //Decrease coins
      //Check if player can buy a card (has 3 coins)
    }

    def placeOnBoard(cardId: Int, cardOrder: Int): IO[PlayerState] = {
      //Move card from hand to board and place it on right place in board list
      //Board can't have more than 7 cards
    }

    def sellCard(cardId: Int): IO[PlayerState] = {
      //Remove card from board list
      //Add one coin to player
    }

    def moveCard(cardId: Int, cardOrder: Int): IO[PlayerState] = {
      //Change order of list
    }

    def rollCards: IO[PlayerState] = {
      //player state roll(hard)
    }

    def frozeCards: IO[PlayerState] = {
      for {
        updatedState <- ref.modify(oldState => {
          val frozenCards = oldState.rolledCards
          val newState = oldState.copy(frozenCards = frozenCards)
          (newState, newState)
        })
      } yield updatedState
    }

    def nextTurn: IO[PlayerState] =
      for {
        updatedState <- ref.modify(oldState => {
          val baseCoins = oldState.baseCoins.increase(1)
          val upgradeCost = oldState.upgradeCost.decrease
          val rolledCards = oldState.roll(false)
          val newState = oldState.copy(baseCoins = baseCoins, coins = baseCoins, upgradeCost = upgradeCost, rolledCards = rolledCards)
          (newState, newState)
        })
      } yield updatedState


    def getCoins: IO[Coins] =
      ref.get.map(_.coins)
  }

  case class PlayerState(name: String,
                         baseCoins: Coins = Coins(3),
                         coins: Coins = Coins(3),
                         level: Level = Level(1),
                         upgradeCost: Cost = Cost(5),
                         availableCards: List[Card] = Tavern.AllCardsByLevel.head,
                         rolledCards: List[Card] = List(),
                         frozenCards: List[Card] = List(),
                         cardsInHand: List[Card] = List()) {

    def roll(hardRoll: Boolean): List[Card] = {
      if (hardRoll) {
        val cardsToTake: Int = 3 + level.value / 2.floor.toInt
        scala.util.Random.shuffle(availableCards).take(cardsToTake)
      } else {
        val cardsToTake: Int = 3 + level.value / 2.floor.toInt - frozenCards.length
        frozenCards ::: scala.util.Random.shuffle(availableCards).take(cardsToTake)
      }

    }

    def updatePool(cards: List[Card], lvl: Level): List[Card] = cards ::: Tavern.AllCardsByLevel(lvl.value - 1)
  }

  val example: IO[Unit] = for {
    ref <- Ref[IO].of(PlayerState(name = "Awe1"))
    playerStates = new PlayerStates(ref)
    playerState <- playerStates.nextTurn
  } yield println(playerState)

  final case class Coins private (value: Int) extends AnyVal {

    def update(value: Int): Option[Coins] = {
      if (value < 0) None
      else if (value > 10) Option(Coins(10))
      else Option(Coins(value))
    }

    def increase(by: Int): Coins = {
      update(value + by) match {
        case Some(x) => x
        case None => Coins(value)
      }
    }

    def buy(cost: Cost): Either[String, Coins] = {
      update(value - cost.value) match {
        case Some(x) => Right(x)
        case None => Left("Insufficient funds")
      }
    }
  }

  object Coins {
    def create(value: Int): Option[Coins] = {
      if (value < 0) None
      else if (value > 10) None
      else Option(Coins(value))
    }
  }

  final case class Level private (value: Int) extends AnyVal {

    def update(value: Int): Option[Level] = {
      if (value < 1) None
      else if (value > 6) None
      else Option(Level(value))
    }

    def increase: Level = {
      update(value + 1) match {
        case Some(x) => x
        case None => Level(value)
      }
    }

    def nextUpgradeBaseCost: Cost = {
      value match {
        case 1 => Cost(5)
        case 2 => Cost(7)
        case 3 => Cost(8)
        case 4 => Cost(9)
        case 5 => Cost(10)
        case 6 => Cost(999)
      }
    }

  }
  object Level {
    def create(value: Int): Option[Level] = {
      if (value < 1) None
      else if (value > 6) None
      else Option(Level(value))
    }
  }

  final case class Cost private (value: Int) extends AnyVal {
    def decrease: Cost = {
      if (value == 0) Cost(value)
      else Cost(value - 1)
    }
  }
}
