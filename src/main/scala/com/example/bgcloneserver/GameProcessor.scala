//package com.example.bgcloneserver
//
//object GameProcessor {
//
//  // Need a timer here
//  // When Timer comes to 0 Game processor sends back to user:
//  // Battle process turn by turn, and a result and next opponent which will be shown to user when the battle ends
//
//  def processCommand(command: CommandADT.Command): Unit = {
//    import com.example.bgcloneserver.TavernState._
//    command.commandName match {
//      case "newPlayer" => Player.newPlayer(command.name.getOrElse("Unnamed player"))
//      case "upgrade" =>
//    }
//  }
//}
