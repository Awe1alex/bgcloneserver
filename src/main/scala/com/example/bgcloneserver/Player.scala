package com.example.bgcloneserver


import cats.effect.IO
import cats.effect.concurrent.Ref
import com.example.bgcloneserver.PlayerADTS._
import com.example.bgcloneserver.CommandADT._

object Player {

  final class PlayerStateRef(ref: Ref[IO, PlayerState]) {

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

    def getDamage(damage: Int): IO[PlayerState] =
      for {
        updatedState <- ref.modify(oldState => {
          val newState = oldState.copy(hp = oldState.hp.getDamage(damage))
          (newState, newState)
        })
      } yield updatedState
  }



   def newPlayer(name: String): IO[PlayerStateRef] = for {
    ref <- Ref[IO].of(PlayerState(name = name))
    playerStates = new PlayerStateRef(ref)
  } yield playerStates

  def processCommand(playerStateRef: PlayerStateRef, command: Command): IO[PlayerState] = {
    command match {
      case Command.Upgrade(_) => playerStateRef.upgradeTavern
      case Command.BuyCard(_, cardId) => playerStateRef.buyCard(cardId)
      case Command.PlaceOnBoard(_, cardId, placeId) => playerStateRef.placeOnBoard(cardId, placeId)
      case Command.SellCard(_, cardId) => playerStateRef.sellCard(cardId)
      case Command.RollCards(_) => playerStateRef.rollCards
      case Command.FreezeCards(_) => playerStateRef.freezeCards
//      case _ =>
    }
  }
}
