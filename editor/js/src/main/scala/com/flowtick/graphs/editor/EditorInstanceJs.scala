package com.flowtick.graphs.editor

import scala.scalajs.js.annotation.JSExport
import cats.effect.unsafe.implicits.global

class EditorInstanceJs(val messageBus: EditorMessageBus) {
  @JSExport
  def execute(commandJson: String): Unit =
    io.circe.parser.decode[EditorCommand](commandJson) match {
      case Right(command) => messageBus.publish(command).unsafeToFuture()
      case Left(error) =>
        println("unable to execute command:", error)
    }

}
