package com.flowtick.graphs

import java.util.UUID

import cats.effect.IO
import cats.implicits._

final case class Action(title: String,
                        shortCut: String,
                        handler: Any => Unit,
                        icon: Option[String] = None)

sealed trait MenuType
case object DropUp extends MenuType
case object Toolbar extends MenuType

final case class EditorMenuSpec(title: String,
                                menuType: MenuType,
                                actions: List[Action],
                                icon: Option[String] = None)

trait EditorMenu extends EditorComponent {
  def messageBus: EditorMessageBus

  lazy val editorMenus: List[EditorMenuSpec] = List (
    EditorMenuSpec("File", Toolbar, actions = List(
      Action("Open File", "ctrl+o", triggerFileOpen, Some("upload")),
      Action("Export JSON", "ctrl+shift+e", triggerExportJson, icon = Some("file-download")),
      Action("Export XML", "ctrl+e", triggerExportXML, Some("file-code"))
    ), icon = Some("file")),
    EditorMenuSpec("Edit", Toolbar, actions = List(
      //Action("Undo", "ctrl+z", (e) => println("undo stuff")),
      Action("Insert Element", "ins", triggerAdd, icon = Some("plus-circle")),
      Action("Delete Selection", "del", triggerDelete, icon = Some("trash")),
      Action("Unselect", "home", triggerUnselect, icon = Some("object-ungroup")),
      Action("Connect Selection", "alt+c", toggleConnect, icon = Some("code-branch"))
    )),
    EditorMenuSpec("View", Toolbar, actions = List(
      Action("Reset View", "ctrl+0", triggerResetView, icon = Some("zoom")),
      Action("Show Palette", "f4", togglePalette, icon = Some("palette")),
      Action("Show Properties", "f2", toggleEdit, icon = Some("edit")),
    ))
  )

  def initMenus: IO[Unit]
  def handleExported(exported: ExportedGraph): IO[Unit]
  def bindShortcut(action: Action): IO[Unit]
  def triggerFileOpen: Any => Unit

  override lazy val eval: Eval = ctx => ctx.effect(this) {
    case exported: ExportedGraph => handleExported(exported)
  }

  override def init(model: EditorModel): IO[Unit] = for {
    _ <- initMenus
    tryBindings <- bindShortcuts.attempt
    _ <- tryBindings match {
      case Right(_) => IO.unit
      case Left(error) => IO(println(s"unable to bind shortcuts? $error", error))
    }
  } yield ()

  def bindShortcuts: IO[Unit] =
    editorMenus.flatMap(_.actions).map(bindShortcut).sequence.void

  def toggleConnect: Any => Unit = _ => {
    messageBus.publish(Toggle(Toggle.connectKey, true)).unsafeRunSync()
  }

  def togglePalette: Any => Unit = _ => {
    messageBus.publish(Toggle(Toggle.paletteKey, true)).unsafeRunSync()
  }

  def toggleEdit: Any => Unit = _ => {
    messageBus.publish(Toggle(Toggle.editKey, true)).unsafeRunSync()
  }

  def triggerUnselect: Any => Unit = _ => {
    messageBus.publish(Select(List.empty)).unsafeRunSync()
  }

  def triggerResetView: Any => Unit = _ => {
    messageBus.publish(Reset).unsafeRunSync()
  }

  def triggerExportXML: Any => Unit = _ => {
    messageBus.publish(Export(GraphMLFormat)).unsafeRunSync()
  }

  def triggerExportJson: Any => Unit = _ => {
    messageBus.publish(Export(JsonFormat)).unsafeRunSync()
  }

  def triggerDelete : Any => Unit = _ => messageBus.publish(DeleteSelection).unsafeRunSync()

  def triggerAdd: Any => Unit = _ => {
    val id = UUID.randomUUID().toString
    messageBus
      .publish(CreateNode(id, None))
      .unsafeRunSync()
  }

}