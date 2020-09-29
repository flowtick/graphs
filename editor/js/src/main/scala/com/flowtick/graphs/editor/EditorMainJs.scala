package com.flowtick.graphs.editor

import cats.effect.IO
import com.flowtick.graphs.graphml.{GraphMLEdge, GraphMLGraph, GraphMLMeta, GraphMLNode, GraphMLResource}
import com.flowtick.graphs.json.JsonGraph
import com.flowtick.graphs.editor.feature.RoutingFeature
import io.circe.Json

import scala.scalajs.js
import scala.scalajs.js.JSON
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import io.circe.syntax._
import io.circe.generic.auto._
import com.flowtick.graphs.json.format.default._
import com.flowtick.graphs.json.schema.Schema
import com.flowtick.graphs.json.schema.JsonSchema._
import org.scalajs.dom.Event

final case class EditorOptionsJs(paletteContainerId: Option[String] = None,
                                 menuContainerId: Option[String] = None,
                                 options: EditorOptions)

@JSExportTopLevel("graphs")
object EditorMainJs extends EditorMain {

  @JSExport
  def createEditor(containerElementId: String,
                   optionsObj: js.UndefOr[js.Object]): EditorInstanceJs = (for {
    options <- optionsObj
      .toOption.map(obj => IO.fromEither(io.circe.parser.decode[EditorOptionsJs](JSON.stringify(obj))))
      .getOrElse(IO.pure(EditorOptionsJs(None, None, EditorOptions())))

    editor <- createEditor(bus => List(
      Some(new EditorPropertiesJs(containerElementId)(bus)),
      Some(new EditorViewJs(containerElementId)(bus)),
      options.paletteContainerId.map(new EditorPaletteJs(_)(bus)),
      options.menuContainerId.map(new EditorMenuJs(_)(bus))
    ).flatten)(options.options)
    (bus, _) = editor
    // we use right click for panning, prevent context menu
    _ <- IO(org.scalajs.dom.window.document.addEventListener("contextmenu", (event: Event) => {
      event.preventDefault()
    }, false))
  } yield new EditorInstanceJs(bus))
    .redeemWith(error => IO(println(s"error while creating editor $error")) *> IO.raiseError(error), IO.pure)
    .unsafeRunSync()

  def main(args: Array[String]): Unit = {
    println("graphs loaded...")
  }
}
