package com.example.bgcloneserver


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

  val level1: List[Card] = Card(Name("first tavern card"), Attack(1), HP(1), Effect(None)) :: Nil

  val level2: List[Card] = Card(Name("second tavern card"),Attack(2), HP(2), Effect(None)) :: Nil

  val level3: List[Card] = Card(Name("third tavern card"),Attack(3), HP(3), Effect(None)) :: Nil

  val level4: List[Card] = Card(Name("fourth tavern card"),Attack(4), HP(4), Effect(None)) :: Nil

  val level5: List[Card] = Card(Name("fifth tavern card"),Attack(5), HP(5), Effect(None)) :: Nil

  val level6: List[Card] = Card(Name("sixth tavern card"),Attack(6), HP(6), Effect(None)) :: Nil

  val AllCardsByLevel: List[List[Card]] = level1 :: level2 :: level3 :: level4 :: level5 :: level6 :: Nil
}
