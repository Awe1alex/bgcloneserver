package com.example.bgcloneserver

import cats.effect.IO
import cats.effect.concurrent.Ref
import com.example.bgcloneserver.Tavern._


object TavernState {
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

    def buyCard(cardId: Int): IO[PlayerState] =
      for {
        updatedState <- ref.modify(oldState => {
          //Check if player can buy a card (has 3 coins)
          oldState.coins.buy(Cost(3)) match {
            case Right(x) =>
              //Decrease coins
              val coins = x
              //Move card from rolled cards to hand
              val (rolledCards, card) = oldState.rolledCards.remove(cardId)
              val cardsInHand = oldState.cardsInHand.add(card)
              val newState = oldState.copy(coins = coins, rolledCards = rolledCards, cardsInHand = cardsInHand)
              (newState, newState)
            case Left(_) =>
              (oldState, oldState)
          }
        })
      } yield updatedState

    def placeOnBoard(cardId: Int, place: Int): IO[PlayerState] = for {
      updatedState <- ref.modify(oldState => {
        //Move card from hand to board
        val (cardsInHand, card) = oldState.cardsInHand.remove(cardId)
        val board = if (oldState.board.value.length < 7) oldState.board.insert(card, place) else oldState.board
        val newState = oldState.copy(cardsInHand = cardsInHand, board = board)
        (newState, newState)
      })
    } yield updatedState

    def sellCard(cardId: Int): IO[PlayerState] = for {
      updatedState <- ref.modify(oldState => {
        //Remove card from board list
        val (board, _) = oldState.board.remove(cardId)
        //Add one coin to player
        val coins = oldState.coins.increase(1)
        val newState = oldState.copy(board = board, coins = coins)
        (newState, newState)
      })
    } yield updatedState

    def moveCard(cardId: Int, place: Int): IO[PlayerState] = for {
      updatedState <- ref.modify(oldState => {
        //Change order of list
        val board = oldState.board.move(cardId, place)
        val newState = oldState.copy(board = board)
        (newState, newState)
      })
    } yield updatedState

    def rollCards: IO[PlayerState] = for {
      updatedState <- ref.modify(oldState => {
        val rolledCards = oldState.roll(true)
        val newState = oldState.copy(rolledCards = rolledCards)
        (newState, newState)
      })
    } yield updatedState

    def freezeCards: IO[PlayerState] = {
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
  }

  case class PlayerState(name: String,
                         baseCoins: Coins = Coins(3),
                         coins: Coins = Coins(3),
                         level: Level = Level(1),
                         upgradeCost: Cost = Cost(5),
                         availableCards: List[Card] = Tavern.AllCardsByLevel.head,
                         rolledCards: CardList = CardList(List()),
                         frozenCards: CardList = CardList(List()),
                         cardsInHand: CardList = CardList(List()),
                         board: CardList = CardList(List())) {

    def roll(hardRoll: Boolean): CardList = {
      if (hardRoll) {
        val cardsToTake: Int = 3 + level.value / 2.floor.toInt
        CardList(scala.util.Random.shuffle(availableCards).take(cardsToTake))
      } else {
        val cardsToTake: Int = 3 + level.value / 2.floor.toInt - frozenCards.value.length
        CardList(frozenCards.value ::: scala.util.Random.shuffle(availableCards).take(cardsToTake))
      }

    }

    def updatePool(cards: List[Card], lvl: Level): List[Card] = cards ::: Tavern.AllCardsByLevel(lvl.value - 1)
  }

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

  final case class CardList(value: List[Card]) {

    def remove(id: Int): (CardList, Card) = {
      (CardList(value.slice(id, id)), value(id))
    }

    def add(card: Card): CardList =
      CardList(value :+ card)

    def insert(card: Card, place: Int): CardList = {
      val (start, end) = value.splitAt(place)
      CardList((start :+ card) ::: end)
    }

    def move(from: Int, to: Int): CardList = {
      val (tempList, card) = CardList(value).remove(from)
      tempList.insert(card, to)
    }

  }
}