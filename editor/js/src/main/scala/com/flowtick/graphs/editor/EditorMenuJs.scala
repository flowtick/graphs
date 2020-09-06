package com.flowtick.graphs.editor

import java.util.UUID

import cats.effect.IO
import com.flowtick.graphs._
import com.flowtick.graphs.editor.vendor.Mousetrap
import com.flowtick.graphs.graphml.{NodeShape, ShapeType}
import org.scalajs.dom.html
import org.scalajs.dom.html.{Div, Input}
import org.scalajs.dom.raw._
import scalatags.JsDom
import scalatags.JsDom.all._

import scala.scalajs.js

final case class Action(title: String,
                        shortCut: String,
                        handler: Any => Unit,
                        icon: Option[String] = None)

sealed trait MenuType
case object DropUp extends MenuType
case object Toolbar extends MenuType

final case class Menu(title: String,
                      menuType: MenuType,
                      actions: List[Action],
                      icon: Option[String] = None)

class EditorMenuJs(menuContainerId: String)(messageBus: EditorMessageBus) extends EditorComponent {
  lazy val menuContainer = org.scalajs.dom.window.document.getElementById(menuContainerId)

  lazy val downloadLink = a(
    id := "download_link",
    href := "", "Click here to download", display := "none"
  ).render

  lazy val fileInput: Input = input(
    `type` := "file",
    id := "file",
    onchange := readFile).render

  lazy val fileInputGroup: Div =
    div(
      cls := "input-group col-2 d-none",
      div (
        cls := "custom-file",
        fileInput,
        label(cls := "custom-file-label", `for` := "file", "Choose file")
      )
    ).render

  lazy val menus: List[Menu] = List (
    Menu("File", Toolbar, actions = List(
      Action("Open File", "ctrl+o", triggerFileOpen, Some("upload")),
      Action("Export JSON", "ctrl+shift+e", triggerExportJson, icon = Some("file-download")),
      Action("Export XML", "ctrl+e", triggerExportXML, Some("file-code"))
    ), icon = Some("file")),
    Menu("Edit", Toolbar, actions = List(
      //Action("Undo", "ctrl+z", (e) => println("undo stuff")),
      Action("Insert Element", "ins", triggerAdd, icon = Some("plus-circle")),
      Action("Delete Selection", "del", triggerDelete, icon = Some("trash")),
      Action("Toggle Properties", "f2", toggleEdit, icon = Some("edit")),
      Action("Unselect", "home", triggerUnselect, icon = Some("object-ungroup")),
      Action("Connect Selection", "alt+c", toggleConnect, icon = Some("code-branch"))
    )),
    Menu("Views", Toolbar, actions = List(
      Action("Show Palette", "f4", togglePalette, icon = Some("palette"))
    ))
  )

  def iconTag(icon: String): JsDom.TypedTag[html.Element] = i(cls := s"fa fa-$icon")

  val navbar: Div = div(
    cls := "collapse navbar-collapse",  id := "menuNavbarCollapse",
    ul(
      cls := "navbar-nav mr-auto"
    ).apply(menus.toArray.map {
      case Menu(menuTitle, Toolbar, actions, _) =>
        div(
          cls := "btn-toolbar",
          role := "toolbar",
          aria.label := menuTitle,
          div(
            cls := "btn-group mr-2",
            role := "group",
          ).apply(actions.toArray.map(action =>
            button(
              `type` := "button",
              cls := "btn btn-secondary",
              data("toggle") := "tooltip",
              data("placement") := "top",
              title := action.title,
              onclick := action.handler,
              action
                .icon.map(iconTag)
                .getOrElse(action.title)
            )
          ))
        )
      case Menu(menuTitle, DropUp, actions, icon) =>
        li(
          cls := "nav-item dropup",
          a(
            cls := "nav-link dropdown-toggle",
            href := "#",
            id := s"menu-$name",
            data("toggle") := "dropdown",
            aria.haspopup :="true",
            aria.expanded := "false",
            icon.map(iconTag).getOrElse(menuTitle)
          ),
          div(
            cls := "dropdown-menu",
            aria.labelledby := s"menu-$name"
          ).apply(actions.toArray.map(action =>
            a(
              cls := "dropdown-item",
              href := "#",
              onclick := action.handler,
              action.icon.map(iconTag).getOrElse(""),
              " ",
              action.title
            )
          ))
        )
    })
  ).render

  def bindShortcuts: IO[Unit] = IO {
    menus.flatMap(_.actions).foreach(action =>
      Mousetrap.bind(action.shortCut, action.handler)
    )
  }

  override def init(model: EditorModel): IO[Unit] = for {
    _ <- IO {
      menuContainer.appendChild(navbar)
      menuContainer.appendChild(fileInputGroup)
    }
    tryBindings <- bindShortcuts.attempt
    _ <- tryBindings match {
      case Right(_) => IO.unit
      case Left(error) => IO(println("unable to bind shortcuts, is Mousetrap present?", error))
    }
  } yield ()

  override lazy val eval: Eval = ctx => ctx.effect(this) {
    case ExportedGraph(name, xml, format) => showLink(name, xml, format)
  }

  def triggerFileOpen: Any => Unit = {
    case _: Event => org.scalajs.dom.document.getElementById(fileInput.id).asInstanceOf[HTMLInputElement].click()
    case _ =>
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

  def readFile: DragEvent => Unit = (event: DragEvent) => {
    val file = event.target.asInstanceOf[HTMLInputElement].files.item(0)
    val format = if (file.name.endsWith(".json")) JsonFormat else GraphMLFormat

    val reader = new FileReader
    reader.onload = (_: Event) => messageBus
      .publish(Load(reader.result.toString, format))
      .unsafeRunSync()
    reader.readAsText(file)
  }

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

  def showLink(name: String, data: String, format: FileFormat): IO[Unit] = IO {
    val blob = new Blob(js.Array.apply(data), BlobPropertyBag("text/plain"));
    val url = URL.createObjectURL(blob)

    val suffix = format match {
      case GraphMLFormat => ".xml"
      case JsonFormat => ".json"
    }

    downloadLink.asInstanceOf[js.Dynamic].download = s"$name$suffix"
    downloadLink.href = url;
    downloadLink.click()
  }

}
