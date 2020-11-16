package com.flowtick.graphs.editor

import cats.effect.IO
import com.flowtick.graphs.style.{ImageSpec, StyleSheet}
import org.scalajs.dom.html.{Button, Div, UList}
import org.scalajs.dom.raw.{Event, HTMLElement}
import scalatags.JsDom.all._

class EditorPaletteJs(paletteElementId: String)(val messageBus: EditorMessageBus) extends EditorPalette {
  lazy val paletteContainer: HTMLElement = org.scalajs.dom.window.document
    .getElementById(paletteElementId)
    .asInstanceOf[HTMLElement]

  def stencilNavList(palette: Palette, styleSheet: StyleSheet): UList = ul(
    cls := "nav",
  ).apply(palette.stencils.toArray.map(group =>
    div(
      cls := "btn-toolbar",
      role := "toolbar",
      aria.label := group.title,
      div(
        cls := "btn-group mr-2",
        role := "group",
      ).apply(group.items.toArray.map(item =>
        button(
          `type` := "button",
          cls := "btn btn-secondary",
          data("toggle") := "tooltip",
          data("placement") := "top",
          title := item.title,
          onclick :=  ((_: Event) => selectPaletteItem(item)),
          ondblclick := ((_: Event) => createPaletteItem(item)),
          imageOrTitle(styleSheet.images, item.previewImageRef, item.title)
        )
      ))
    ))).render

  def connectorNavList(palette: Palette, styleSheet: StyleSheet): UList = ul(
    cls := "nav",
  ).apply(palette.connectors.toArray.map(group =>
    div(
      cls := "btn-toolbar",
      role := "toolbar",
      aria.label := group.title,
      div(
        cls := "btn-group mr-2",
        role := "group",
      ).apply(group.items.toArray.map(item => {
        button(
          `type` := "button",
          cls := "btn btn-secondary",
          data("toggle") := "tooltip",
          data("placement") := "top",
          title := item.title,
          onclick := ((_: Event) => selectConnectorItem(item)),
          imageOrTitle(styleSheet.images, item.previewImageRef, item.title)
        )
      }
      ))
    ))).render

  private def imageOrTitle(images: Map[String, ImageSpec], imageRef: Option[String], title: String): Modifier = {
    imageRef.flatMap(images.get).map {
      case ImageSpec(data, "dataUrl") => img(src := data, height := 32)
      case ImageSpec(url, "url") => img(src := url, height := 32)
      case ImageSpec(url, _) => img(src := url, height := 32)
    }.getOrElse(title)
  }

  def stencilsElement(palette: Palette, styleSheet: StyleSheet): Div = {
    div(
      cls := "collapse navbar-collapse",
      id := "paletteNavbarCollapse",
      stencilNavList(palette, styleSheet)
    )
  }.render

  def connectorsElement(palette: Palette, styleSheet: StyleSheet): Div = {
    div(
      cls := "collapse navbar-collapse",
      id := "connectorNavbarCollapse",
      connectorNavList(palette, styleSheet)
    )
  }.render

  override def toggleView(enabled: Boolean): IO[Boolean] = IO {
    if (enabled) {
      paletteContainer.classList.remove("d-none")
    } else {
      paletteContainer.classList.add("d-none")
    }
    enabled
  }

  lazy val closeButton: Button =
    button(
      cls := "close float-right",
      `type` := "button",
      data("dismiss") := "modal",
      aria.label := "Close",
      span(aria.hidden := "true", "×"),
      onclick := ((_: Event) => messageBus.publish(EditorToggle(EditorToggle.paletteKey, Some(false))).unsafeRunSync())
    ).render

  override def initPalette(model: EditorModel): IO[Unit] = for {
    newPaletteElement <- IO.pure(stencilNavList(model.palette, model.editorGraph.styleSheet))
    newConnectorsElement <- IO.pure(connectorNavList(model.palette, model.editorGraph.styleSheet))
    _ <- IO {
      paletteContainer.appendChild(newPaletteElement)
      paletteContainer.appendChild(newConnectorsElement)
    }
    _ <- IO(paletteContainer.appendChild(closeButton))
  } yield ()
}
