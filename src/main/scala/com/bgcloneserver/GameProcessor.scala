//package com.bgcloneserver
//
//import cats.effect.Timer
//import com.bgcloneserver.PlayerADTS.PlayerState
//import com.bgcloneserver.Tavern.{Card, HP}
//
//object GameProcessor {
//
//
////  val timer: Timer.type = Timer
//
//  def processBattle(playerState1: PlayerState, playerState2: PlayerState): (PlayerState, PlayerState) = {
//    val board1 = playerState1.board
//    val board2 = playerState2.board
//  }
//
//  def attack(cardId: Int, attacker: List[Card], defender: List[Card]) = {
//    val attackerCard = attacker(cardId)
//    val defenderId = scala.util.Random.nextInt(defender.length)
//    val defenderCard = defender(defenderId)
//  }
//
//  def cardDamage (from: Card, to: Card): (Option[Card], Option[Card]) = {
//    val fromHp = HP(from.hp.value - to.attack.value)
//    val toHp = HP(to.hp.value - to.attack.value)
//  }
//}
//
