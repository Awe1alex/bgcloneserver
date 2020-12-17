package com.example.bgcloneserver

import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._

object CommandADT {

  final case class Command(commandName: String, arguments: Option[List[Int]])

  implicit val commandDecoder: Decoder[Command] = deriveDecoder
  implicit val commandEncoder: Encoder[Command] = deriveEncoder

}

//  sealed trait Command
//  final case class Upgrade() extends Command
//  final case class BuyCard(cardId: Int) extends Command
//  final case class PlaceOnBoard(cardId: Int, place: Int) extends Command
//  final case class SellCard(cardId: Int) extends Command
//  final case class RollCards() extends Command
//  final case class FreezeCards() extends Command
//  final case class CommandName(value: String) extends AnyVal
//  object CommandName {
//    def create(value: String): Option[CommandName] = {
//      value match {
//        case "upgrade" => CommandName("upgrade")
//      }
//    }
//  }