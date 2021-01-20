package com.bgcloneserver

import cats.effect.IO
import cats.effect.concurrent.Ref
import com.bgcloneserver.CommandADT.Command
import com.bgcloneserver.PlayerADTS.Player
import PlayerADTS._

object GameState {
  final class PlayerStates(ref: Ref[IO, Map[String, Player]]) {

    def processCommand(command: Command): Option[IO[Option[Player]]] = {
      command match {
        case Command.Upgrade(playerName) => Some(upgradeTavern(playerName))

        case Command.BuyCard(playerName, cardId) => Some(buyCard(playerName, cardId))

        case Command.PlaceOnBoard(playerName, cardId, placeId) => Some(placeOnBoard(playerName,cardId, placeId))

        case Command.SellCard(playerName, cardId) => Some(sellCard(playerName, cardId))

        case Command.RollCards(playerName) => Some(rollCards(playerName))

        case Command.FreezeCards(playerName) => Some(freezeCards(playerName))

        case _ => None
      }
    }

    def addPlayer(player: Player): IO[Unit] =
      ref.update(_ + (player.name -> player))

    def upgradeTavern(playerName: String): IO[Option[Player]] =
      for {
        updatedState <- ref.modify(
        allPlayers => {
          val maybePlayer = allPlayers.get(playerName).map {
            player => {
              if (player.level.value <= 6) {
                player.coins.buy(player.upgradeCost) match {
                  case Right(x) =>
                    val coins = x
                    val level = player.level.increase
                    val upgradeCost = level.nextUpgradeBaseCost
                    val availableCards = player.updatePool(player.availableCards, level)
                    val newPlayer = player.copy(coins = coins, level = level, upgradeCost = upgradeCost, availableCards = availableCards)
                    newPlayer
                  case Left(_) =>
                    player
                }
              } else {
                player
              }
            }
          }
          val newPlayers = allPlayers ++ maybePlayer.map(p => (p.name, p))
          (newPlayers, maybePlayer)
        })
      } yield updatedState

    def buyCard(playerName: String, cardId: Int): IO[Option[Player]] =
      for {
        updatedState <- ref.modify(allPlayers => {
          val maybePlayer = allPlayers.get(playerName).map {
            player => {
              //Check if player can buy a card (has 3 coins)
              player.coins.buy(Cost(3)) match {
                case Right(x) =>
                  //Decrease coins
                  val coins = x
                  //Move card from rolled cards to hand
                  println(player.rolledCards.remove(cardId))
                  val (rolledCards, card) = player.rolledCards.remove(cardId)
                  val cardsInHand = player.cardsInHand.add(card)
                  val newPlayer = player.copy(coins = coins, rolledCards = rolledCards, cardsInHand = cardsInHand)
                  newPlayer
                case Left(_) =>
                  player
              }
            }
          }
          val newPlayers = allPlayers ++ maybePlayer.map(p => (p.name, p))
          (newPlayers, maybePlayer)
        })
      } yield updatedState

    def placeOnBoard(playerName: String, cardId: Int, place: Int): IO[Option[Player]] = for {
      updatedState <- ref.modify(
      allPlayers => {
        val maybePlayer = allPlayers.get(playerName).map { player =>
          //Move card from hand to board
          val (cardsInHand, card) = player.cardsInHand.remove(cardId)
          val board = if (player.board.value.length < 7) player.board.insert(card, place) else player.board
          val newPlayer = player.copy(cardsInHand = cardsInHand, board = board)
          newPlayer
        }
        val newPlayers = allPlayers ++ maybePlayer.map(p => (p.name, p))

        (newPlayers, maybePlayer)
      })
    } yield updatedState

    def sellCard(playerName: String, cardId: Int): IO[Option[Player]] = for {
      updatedState <- ref.modify(
        allPlayers => {
        val maybePlayer = allPlayers.get(playerName).map { player =>
          //Remove card from board list
          val (board, _) = player.board.remove(cardId)
          //Add one coin to player
          val coins = player.coins.increase(1)
          val newPlayer = player.copy(board = board, coins = coins)
          newPlayer
        }
        val newPlayers = allPlayers ++ maybePlayer.map(p => (p.name, p))

        (newPlayers, maybePlayer)
      })
    } yield updatedState

    def moveCard(playerName: String, cardId: Int, place: Int): IO[Option[Player]] = for {
      updatedState <- ref.modify(
        allPlayers => {
          val maybePlayer = allPlayers.get(playerName).map { player =>
            //Change order of list
            val board = player.board.move(cardId, place)
            val newPlayer = player.copy(board = board)
            newPlayer
          }
          val newPlayers = allPlayers ++ maybePlayer.map(p => (p.name, p))

          (newPlayers, maybePlayer)
      })
    } yield updatedState

    def rollCards(playerName: String): IO[Option[Player]] = for {
      updatedState <- ref.modify(
        allPlayers => {
          val maybePlayer = allPlayers.get(playerName).map { player =>
            val rolledCards = player.roll(true)
            val newPlayer = player.copy(rolledCards = rolledCards)
            newPlayer
          }
          val newPlayers = allPlayers ++ maybePlayer.map(p => (p.name, p))

          (newPlayers, maybePlayer)
      })
    } yield updatedState

    def freezeCards(playerName: String): IO[Option[Player]] = {
      for {
        updatedState <- ref.modify(
          allPlayers => {
            val maybePlayer = allPlayers.get(playerName).map { player =>
              val frozenCards = player.rolledCards
              val newPlayer = player.copy(frozenCards = frozenCards)
              newPlayer
            }
            val newPlayers = allPlayers ++ maybePlayer.map(p => (p.name, p))

            (newPlayers, maybePlayer)
        })
      } yield updatedState
    }

    def nextTurn: IO[Unit] =
      for {
        _ <- ref.modify(
          allPlayers => {
            val newPlayers = allPlayers.view.mapValues(player => {
              val baseCoins = player.baseCoins.increase(1)
              val upgradeCost = player.upgradeCost.decrease
              val rolledCards = player.roll(false)
              val newPlayer = player.copy(baseCoins = baseCoins, coins = baseCoins, upgradeCost = upgradeCost, rolledCards = rolledCards)
              newPlayer
            }).toMap
            (newPlayers, newPlayers)
          })
      } yield ()

    def getDamage(playerName: String, damage: Int): IO[Option[Player]] =
      for {
        updatedState <- ref.modify(
          allPlayers => {val maybePlayer = allPlayers.get(playerName).map { player =>
            val newPlayer = player.copy(hp = player.hp.getDamage(damage))
            newPlayer
          }
          val newPlayers = allPlayers ++ maybePlayer.map(p => (p.name, p))

          (newPlayers, maybePlayer)
        })
      } yield updatedState

    def getValue: PlayerStates = new PlayerStates(ref)
  }

  def newPlayer(playerStates: PlayerStates, playerName: String): IO[Unit] =  {
   playerStates.addPlayer(Player(name = playerName))
  }
}
