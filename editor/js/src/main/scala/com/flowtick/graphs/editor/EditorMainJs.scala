package com.flowtick.graphs.editor

import cats.effect.IO
import com.flowtick.graphs.graphml.{GraphMLEdge, GraphMLGraph, GraphMLMeta, GraphMLNode, GraphMLResource}
import com.flowtick.graphs.json.JsonGraph
import com.flowtick.graphs.{EditorMain, EditorModel, EditorModelUpdate, Palette, RoutingFeature}
import io.circe.Json

import scala.scalajs.js
import scala.scalajs.js.JSON
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import io.circe.syntax._
import io.circe.generic.auto._
import com.flowtick.graphs.json.format.default._
import com.flowtick.graphs.json.schema.Schema
import com.flowtick.graphs.json.schema.JsonSchema._

final case class EditorOptions(paletteContainerId: Option[String] = None,
                               menuContainerId: Option[String] = None,
                               palette: Option[Palette] = None,
                               initial: Option[JsonGraph[Json, GraphMLEdge[Json], GraphMLNode[Json]]] = None,
                               schema: Option[EditorModel.EditorSchema] = None)

@JSExportTopLevel("graphs")
object EditorMainJs extends EditorMain {

  @JSExport
  def createEditor(containerElementId: String,
                   optionsObj: js.UndefOr[js.Object]): EditorInstanceJs = (for {
    options <- optionsObj
      .toOption.map(obj => IO.fromEither(io.circe.parser.decode[EditorOptions](JSON.stringify(obj))))
      .getOrElse(IO.pure(EditorOptions()))

    editor <- createEditor(bus => List(
      Some(new RoutingFeature),
      Some(new EditorModelUpdate),
      Some(new EditorPropertiesJs(containerElementId)(bus)),
      Some(new EditorViewJs(containerElementId)(bus)),
      options.paletteContainerId.map(new EditorPaletteJs(_)(bus)),
      options.menuContainerId.map(new EditorMenuJs(_)(bus))
    ).flatten)(options.initial.map(jsonGraph => GraphMLGraph[Json, Json](
      jsonGraph.graph,
      GraphMLMeta(None, Seq.empty, resources = Seq.empty))),
      options.palette,
      options.schema
    )
    (bus, _) = editor
  } yield new EditorInstanceJs(bus))
    .redeemWith(error => IO(println(s"error while creating editor $error")) *> IO.raiseError(error), IO.pure)
    .unsafeRunSync()

  def main(args: Array[String]): Unit = {
    println("graphs loaded...")
  }
}
