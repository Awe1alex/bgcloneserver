package com.bgcloneserver

import io.circe.generic.semiauto._
import io.circe.{ Decoder, Encoder }, io.circe.generic.auto._
//import io.circe.syntax._
//import io.circe.parser._

object CommandADT {

  sealed trait Command
  object Command {
    final case class Upgrade(playerName: String) extends Command
    final case class BuyCard(playerName: String, cardId: Int) extends Command
    final case class PlaceOnBoard(playerName: String, cardId: Int, placeId: Int) extends Command
    final case class SellCard(playerName: String, cardId: Int) extends Command
    final case class RollCards(playerName: String) extends Command
    final case class FreezeCards(playerName: String) extends Command
  }

  object CommandJson {
    implicit val commandDecoder: Decoder[Command] = deriveDecoder
    implicit val commandEncoder: Encoder[Command] = deriveEncoder
  }

//  val example = Command.Upgrade("Awe1").asJson
//
//  val decoded = for {
//    parsedJson <- parse("{ \"Upgrade\": { \"playerName\" : \"Awe1\" } }")
//    parsedCommand <- parsedJson.as[Command]
//  } yield parsedCommand

}