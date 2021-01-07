package com.flowtick.graphs.editor

import cats.effect.IO
import org.scalajs.dom.Event

import scala.scalajs.js
import scala.scalajs.js.{JSON, undefined}
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("graphs")
object EditorMainJs extends EditorMain {

  @JSExport
  def createEditor(containerElementId: String,
                   optionsObj: js.UndefOr[js.Object],
                   menuContainerId: js.UndefOr[String] = undefined,
                   paletteContainerId: js.UndefOr[String] = undefined): EditorInstanceJs = (for {
    options <- optionsObj
      .toOption
      .map(obj => IO.fromEither(EditorConfiguration.decode(obj.toString)))
      .getOrElse(IO.pure(EditorConfiguration()))

    editor <- createEditor(bus => List(
      Some(new EditorPropertiesJs(containerElementId)(bus)),
      Some(new EditorViewJs(containerElementId)(bus)),
      paletteContainerId.map(new EditorPaletteJs(_)(bus)).toOption,
      menuContainerId.map(new EditorMenuJs(_)(bus)).toOption
    ).flatten)(options)
    // we use right click for panning, prevent context menu
    _ <- IO(org.scalajs.dom.window.document.addEventListener("contextmenu", (event: Event) => {
      event.preventDefault()
    }, false))
  } yield new EditorInstanceJs(editor.bus))
    .redeemWith(error => IO(println(s"error while creating editor $error")) *> IO.raiseError(error), IO.pure)
    .unsafeRunSync()

  def main(args: Array[String]): Unit = {
    println("graphs loaded...")
  }
}
