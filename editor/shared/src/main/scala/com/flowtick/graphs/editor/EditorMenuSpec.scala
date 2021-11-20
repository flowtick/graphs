package com.flowtick.graphs.editor

import java.util.UUID
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits._
import com.flowtick.graphs.graphml.{GraphML, GraphMLGraph}
import com.flowtick.graphs.view.ViewComponent

final case class Action(
    title: String,
    shortCut: String,
    handler: Any => Unit,
    icon: Option[String] = None
) {
  def fullTitle = s"$title [$shortCut]"
}

sealed trait MenuType
case object DropUp extends MenuType
case object Toolbar extends MenuType

final case class EditorMenuSpec(
    title: String,
    menuType: MenuType,
    actions: List[Action],
    icon: Option[String] = None
)

trait EditorMenu extends EditorComponent {
  def messageBus: EditorMessageBus

  lazy val editorMenus: List[EditorMenuSpec] = List(
    EditorMenuSpec(
      "File",
      Toolbar,
      actions = List(
        Action("New File", "ctrl+n", triggerFileNew, Some("file")),
        Action("Open File", "ctrl+o", triggerFileOpen, Some("upload")),
        Action(
          "Export JSON",
          "ctrl+shift+e",
          triggerExportJson,
          icon = Some("file-download")
        ),
        Action("Export XML", "ctrl+e", triggerExportXML, Some("file-code"))
      ),
      icon = Some("file")
    ),
    EditorMenuSpec(
      "Edit",
      Toolbar,
      actions = List(
        Action("Undo", "ctrl+z", triggerUndo, icon = Some("undo")),
        Action("Insert Element", "ins", triggerAdd, icon = Some("plus-circle")),
        Action(
          "Select All",
          "alt+a",
          triggerSelectAll,
          icon = Some("object-group")
        ),
        Action(
          "Unselect",
          "alt+shift+a",
          triggerUnselect,
          icon = Some("object-ungroup")
        ),
        Action("Delete Selection", "del", triggerDelete, icon = Some("trash")),
        Action(
          "Connect Selection",
          "alt+c",
          toggleConnect,
          icon = Some("code-branch")
        )
      )
    ),
    EditorMenuSpec(
      "View",
      Toolbar,
      actions = List(
        Action(
          "Reset View",
          "ctrl+0",
          triggerResetView,
          icon = Some("search-location")
        ),
        Action("Show Palette", "f4", togglePalette, icon = Some("palette")),
        Action("Show Properties", "f2", toggleEdit, icon = Some("edit"))
      )
    )
  )

  def initMenus: IO[Unit]
  def handleExported(exported: ExportedGraph): IO[Unit]
  def bindShortcut(action: Action): IO[Unit]
  def triggerFileOpen: Any => Unit

  override lazy val eval: Eval = ctx =>
    ctx.effect(this) { case exported: ExportedGraph =>
      handleExported(exported)
    }

  override def init(model: EditorModel): IO[Unit] = for {
    _ <- initMenus
    tryBindings <- bindShortcuts.attempt
    _ <- tryBindings match {
      case Right(_) => IO.unit
      case Left(error) =>
        IO(println(s"unable to bind shortcuts? $error", error))
    }
  } yield ()

  def bindShortcuts: IO[Unit] =
    editorMenus.flatMap(_.actions).map(bindShortcut).sequence.void

  def toggleConnect: Any => Unit = _ => {
    messageBus
      .publish(EditorToggle(EditorToggle.connectKey, Some(true)))
      .unsafeToFuture()
  }

  def triggerFileNew: Any => Unit = _ => {
    messageBus.publish(Reset).unsafeToFuture()
  }

  def togglePalette: Any => Unit = _ => {
    messageBus
      .publish(EditorToggle(EditorToggle.paletteKey, None))
      .unsafeToFuture()
  }

  def toggleEdit: Any => Unit = _ => {
    messageBus.publish(EditorToggle(EditorToggle.editKey, None)).unsafeToFuture()
  }

  def triggerUnselect: Any => Unit = _ => {
    messageBus.publish(Select(Set.empty)).unsafeToFuture()
  }

  def triggerResetView: Any => Unit = _ => {
    messageBus.publish(ResetTransformation).unsafeToFuture()
  }

  def triggerExportXML: Any => Unit = _ => {
    messageBus.publish(Export(GraphMLFormat)).unsafeToFuture()
  }

  def triggerExportJson: Any => Unit = _ => {
    messageBus.publish(Export(JsonFormat)).unsafeToFuture()
  }

  def triggerDelete: Any => Unit = _ => messageBus.publish(DeleteSelection).unsafeToFuture()
  def triggerUndo: Any => Unit = _ => messageBus.publish(Undo).unsafeToFuture()
  def triggerSelectAll: Any => Unit = _ => messageBus.publish(SelectAll).unsafeToFuture()

  def triggerAdd: Any => Unit = _ => {
    val id = UUID.randomUUID().toString
    messageBus
      .publish(AddNode(id, None))
      .unsafeToFuture()
  }

}
