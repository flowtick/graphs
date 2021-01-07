package com.flowtick.graphs.editor

import com.flowtick.graphs.style.EdgePath
import io.circe.Decoder.Result
import io.circe.{Decoder, DecodingFailure, HCursor, Json}

case class AddNode(id: String,
                   stencilRef: Option[String] = None,
                   x: Option[Double] = None,
                   y: Option[Double] = None) extends EditorCommand

case class SetLabel(elementRef: ElementRef, label: String) extends EditorCommand
case class SetColor(elementRef: ElementRef, color: String) extends EditorCommand
case class SetJsonString(elementRef: ElementRef, json: String) extends EditorCommand
case class SetJson(elementRef: ElementRef, json: Json => Json) extends EditorCommand

case class Load(value: String, format: FileFormat) extends EditorCommand
case object Reset extends EditorCommand
case class SetModel(model: EditorModel) extends EditorCommand

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

case class MoveTo(ref: ElementRef, x: Double, y: Double) extends EditorCommand
case class MoveBy(deltaX: Double, deltaY: Double) extends EditorCommand

case class AddEdge(id: String,
                   from: String,
                   to: String,
                   connectorRef: Option[String] = None,
                   path: Option[EdgePath] = None) extends EditorCommand

case class ElementRef(id: String, elementType: ElementType)
case class ElementUpdated(element: ElementRef, update: UpdateType = Changed, causedBy: Option[EditorEvent] = None) extends EditorEvent

case object ResetTransformation extends EditorCommand
case class Select(selection: Set[ElementRef], append: Boolean = false) extends EditorCommand
case class Selected(elements: Set[ElementRef], oldSelection: Set[ElementRef]) extends EditorEvent

case object DeleteSelection extends EditorCommand
case class EditorToggle(key: String, value: Option[Boolean]) extends EditorCommand
case object Undo extends EditorCommand
case object SelectAll extends EditorCommand

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

// updates that are only used for internal communication between components
// these should not be considered for undo etc.
// TODO: maybe have more specialized events to trigger wanted behaviour like redraw?
case object Internal extends UpdateType

object EditorToggle {
  val connectKey = "connect"
  val editKey = "edit"
  val paletteKey = "palette"
}

object EditorCommand {
  implicit val decoder = new Decoder[EditorCommand] {
    import io.circe.generic.auto._

    def convert[A](c: HCursor)(implicit decoder: Decoder[A]) =
      c.downField("name").delete.as[A]

    override def apply(c: HCursor): Result[EditorCommand] = c
      .downField("name")
      .as[String] match {
      case Right("add-node") => convert[AddNode](c)
      case _ => Left(DecodingFailure("unknown command name", List.empty))
    }
  }
}