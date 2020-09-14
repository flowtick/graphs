package com.flowtick.graphs

import com.flowtick.graphs.graphml.GraphMLGraph
import io.circe.Decoder.Result
import io.circe.generic.auto._
import io.circe.{Decoder, DecodingFailure, HCursor, Json}

case class CreateNode(id: String,
                      stencilRef: Option[String] = None,
                      x: Option[Double] = None,
                      y: Option[Double] = None) extends EditorCommand

case class SetLabel(elementRef: ElementRef, label: String) extends EditorCommand
case class SetJsonString(elementRef: ElementRef, json: String) extends EditorCommand
case class SetJson(elementRef: ElementRef, json: Json => Json) extends EditorCommand

case class Load(value: String, format: FileFormat) extends EditorCommand
case class SetGraph(graphml: GraphMLGraph[Json, Json]) extends EditorCommand

sealed trait FileFormat {
  def extension: String
}

case object GraphMLFormat extends FileFormat {
  override def `extension`: String = ".graphml"
}
case object JsonFormat extends FileFormat {
  override def `extension`: String = ".json"
}

case class Export(format: FileFormat) extends EditorCommand
case class ExportedGraph(name: String, value: String, format: FileFormat) extends EditorEvent

case class Move(ref: ElementRef, x: Double, y: Double) extends EditorCommand

case class AddEdge(id: String,
                   from: String,
                   to: String,
                   stencilRef: Option[String] = None) extends EditorCommand

case class ElementRef(id: String, elementType: ElementType)
case class ElementUpdated(element: ElementRef, update: UpdateType = Changed) extends EditorEvent

case object Reset extends EditorCommand
case class Select(selection: Seq[ElementRef]) extends EditorCommand
case class Selected(elements: Seq[ElementRef], oldSelection: Seq[ElementRef]) extends EditorEvent

case object DeleteSelection extends EditorCommand
case class Toggle(key: String, value: Boolean) extends EditorCommand

case class EditorErrorMessage(message: String) extends EditorEvent

sealed trait EditorEvent
sealed trait EditorCommand extends EditorEvent

sealed trait ElementType
case object NodeType extends ElementType
case object EdgeType extends ElementType

sealed trait UpdateType
case object Created extends UpdateType
case object Changed extends UpdateType
case object Deleted extends UpdateType

object Toggle {
  val connectKey = "connect"
  val editKey = "edit"
  val paletteKey = "palette"
}

object EditorCommand {
  implicit val decoder = new Decoder[EditorCommand] {
    def convert[A](c: HCursor)(implicit decoder: Decoder[A]) =
      c.downField("name").delete.as[A]

    override def apply(c: HCursor): Result[EditorCommand] = c
      .downField("name")
      .as[String] match {
      case Right("add-node") => convert[CreateNode](c)
      case _ => Left(DecodingFailure("unknown command name", List.empty))
    }
  }
}