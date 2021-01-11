package com.bgcloneserver

object Tavern {
  // Tavern Adts
  // 1. Card (Attack, HP, Effect)
  // 2. Level (List[Card])

  final case class Name(value: String) extends AnyVal

  final case class Attack private (value: Int) extends AnyVal
  object Attack {
    def create(value: Int): Option[Attack] =
      if (value < 0) None
      else Option(Attack(value))
  }

  final case class HP private (value: Int) extends AnyVal
  object HP {
    def create(value: Int): Option[HP] = {
      if (value < 0) None
      else Option(HP(value))
    }
  }

  final case class Effect(value: Option[String])

  final case class Card(name: Name, attack: Attack, hp: HP, effect: Effect)

//  final case class CardsLevel(value: List[Card])

  // Tabulate is used to make card pool as it is in game, so it is not endless

  val level1: List[Card] =
    List.tabulate(16)(_ => Card(Name("first tavern card"), Attack(2), HP(1), Effect(None))) :::
    List.tabulate(16)(_ => Card(Name("first tavern card 2"), Attack(1), HP(3), Effect(None)))

  val level2: List[Card] =
    List.tabulate(15)(_ => Card(Name("second tavern card"),Attack(2), HP(2), Effect(None)))

  val level3: List[Card] =
    List.tabulate(13)(_ => Card(Name("third tavern card"),Attack(3), HP(3), Effect(None)))

  val level4: List[Card] =
    List.tabulate(11)(_ => Card(Name("fourth tavern card"),Attack(4), HP(4), Effect(None)))

  val level5: List[Card] =
    List.tabulate(9)(_ => Card(Name("fifth tavern card"),Attack(5), HP(5), Effect(None)))

  val level6: List[Card] =
    List.tabulate(7)(_ => Card(Name("sixth tavern card"),Attack(6), HP(6), Effect(None)))

  val AllCardsByLevel: List[List[Card]] = level1 :: level2 :: level3 :: level4 :: level5 :: level6 :: Nil
}
