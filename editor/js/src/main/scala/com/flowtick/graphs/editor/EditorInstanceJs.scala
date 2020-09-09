package com.flowtick.graphs.editor

import com.flowtick.graphs.{EditorCommand, EditorMessageBus}

import scala.scalajs.js.annotation.JSExport

class EditorInstanceJs(val messageBus: EditorMessageBus) {
  @JSExport
  def execute(commandJson: String): Unit =
    io.circe.parser.decode[EditorCommand](commandJson) match {
      case Right(command) => messageBus.publish(command).unsafeRunSync()
      case Left(error) =>
        println("unable to execute command:", error)
    }

}
