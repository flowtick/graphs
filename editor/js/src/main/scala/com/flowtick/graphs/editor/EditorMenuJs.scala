package com.flowtick.graphs.editor

import cats.effect.IO
import com.flowtick.graphs._
import com.flowtick.graphs.editor.vendor.Mousetrap
import org.scalajs.dom.html
import org.scalajs.dom.html.{Div, Input}
import org.scalajs.dom.raw._
import scalatags.JsDom
import scalatags.JsDom.all._

import scala.scalajs.js

class EditorMenuJs(menuContainerId: String)(val messageBus: EditorMessageBus) extends EditorMenu {
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

  def iconTag(icon: String): JsDom.TypedTag[html.Element] = i(cls := s"fa fa-$icon")

  val navbar: Div = div(
    cls := "collapse navbar-collapse",  id := "menuNavbarCollapse",
    ul(
      cls := "navbar-nav mr-auto"
    ).apply(editorMenus.toArray.map {
      case EditorMenuSpec(menuTitle, Toolbar, actions, _) =>
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
      case EditorMenuSpec(menuTitle, DropUp, actions, icon) =>
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

  override def bindShortcut(action: Action): IO[Unit] = IO {
    Mousetrap.bind(action.shortCut, action.handler)
  }

  override def handleExported(exported: ExportedGraph): IO[Unit] = showLink(exported.name, exported.value, exported.format)

  def readFile: DragEvent => Unit = (event: DragEvent) => {
    val file = event.target.asInstanceOf[HTMLInputElement].files.item(0)
    val format = if (file.name.endsWith(".json")) JsonFormat else GraphMLFormat

    val reader = new FileReader
    reader.onload = (_: Event) => messageBus
      .publish(Load(reader.result.toString, format))
      .unsafeRunSync()
    reader.readAsText(file)
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

  override def triggerFileOpen: Any => Unit = {
    case _: Event => org.scalajs.dom.document.getElementById(fileInput.id).asInstanceOf[HTMLInputElement].click()
    case _ =>
  }

  override def initMenus: IO[Unit] = IO {
    menuContainer.appendChild(navbar)
    menuContainer.appendChild(fileInputGroup)
  }

}
