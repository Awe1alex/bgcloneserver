package com.example.bgcloneserver

import com.example.bgcloneserver.Tavern._


object PlayerADTS {
  // Player ADTs to implement:
  // 1. Nickname
  // 2. HP
  // 3. Cards on board
  // 5. Tavern State (incl cards in hand)
  // 6. Hero (in future)
  case class PlayerState(name: String,
                         baseCoins: Coins = Coins(3),
                         coins: Coins = Coins(3),
                         level: Level = Level(1),
                         upgradeCost: Cost = Cost(5),
                         availableCards: List[Card] = Tavern.AllCardsByLevel.head,
                         rolledCards: CardList = CardList(List()),
                         frozenCards: CardList = CardList(List()),
                         cardsInHand: CardList = CardList(List()),
                         board: CardList = CardList(List()),
                         hp: HP = HP(40)
                        ) {

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

  final case class HP private (value: Int) extends AnyVal {
    def getDamage(damage: Int): HP = {
      HP(value - damage)
    }
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