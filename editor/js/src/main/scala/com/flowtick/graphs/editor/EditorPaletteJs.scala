package com.flowtick.graphs.editor

import cats.effect.IO
import com.flowtick.graphs.editor.feature.PaletteFeature
import com.flowtick.graphs.style.ImageSpec
import org.scalajs.dom.html.{Button, Div, UList}
import org.scalajs.dom.raw.{Event, HTMLElement}
import scalatags.JsDom.all._
import cats.effect.unsafe.implicits.global

class EditorPaletteJs(paletteElementId: String)(
    val messageBus: EditorMessageBus
) extends PaletteFeature {
  lazy val paletteContainer: HTMLElement = org.scalajs.dom.window.document
    .getElementById(paletteElementId)
    .asInstanceOf[HTMLElement]

  def stencilNavList(palette: Palette): UList = ul(
    cls := "nav"
  ).apply(
    palette.stencils.toArray.map(group =>
      div(
        cls := "btn-toolbar",
        role := "toolbar",
        aria.label := group.title,
        div(
          cls := "btn-group mr-2",
          role := "group"
        ).apply(
          group.items.toArray.map(item =>
            button(
              `type` := "button",
              cls := "btn btn-secondary",
              data("toggle") := "tooltip",
              data("placement") := "top",
              title := item.title,
              onclick := ((_: Event) => selectPaletteItem(item)),
              ondblclick := ((_: Event) => createFromStencil(item)),
              imageOrTitle(item.image, item.title)
            )
          )
        )
      )
    )
  ).render

  def connectorNavList(palette: Palette): UList = ul(
    cls := "nav"
  ).apply(
    palette.connectors.toArray.map(group =>
      div(
        cls := "btn-toolbar",
        role := "toolbar",
        aria.label := group.title,
        div(
          cls := "btn-group mr-2",
          role := "group"
        ).apply(group.items.toArray.map(item => {
          button(
            `type` := "button",
            cls := "btn btn-secondary",
            data("toggle") := "tooltip",
            data("placement") := "top",
            title := item.title,
            onclick := ((_: Event) => selectConnectorItem(item)),
            imageOrTitle(item.image, item.title)
          )
        }))
      )
    )
  ).render

  private def imageOrTitle(
      imageSpec: Option[ImageSpec],
      title: String
  ): Modifier =
    imageSpec
      .map {
        case ImageSpec(data, "dataUrl", _, heightOpt) =>
          img(src := data, height := heightOpt.getOrElse(32.0).toInt)
        case ImageSpec(url, "url", _, heightOpt) =>
          img(src := url, height := heightOpt.getOrElse(32.0).toInt)
        case ImageSpec(url, _, _, heightOpt) =>
          img(src := url, height := heightOpt.getOrElse(32.0).toInt)
      }
      .getOrElse(title)

  def stencilsElement(palette: Palette): Div = {
    div(
      cls := "collapse navbar-collapse",
      id := "paletteNavbarCollapse",
      stencilNavList(palette)
    )
  }.render

  def connectorsElement(palette: Palette): Div = {
    div(
      cls := "collapse navbar-collapse",
      id := "connectorNavbarCollapse",
      connectorNavList(palette)
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
      onclick := ((_: Event) =>
        messageBus
          .publish(EditorToggle(EditorToggle.paletteKey, Some(false)))
          .unsafeToFuture()
      )
    ).render

  override def initPalette(model: EditorModel): IO[Unit] = for {
    newPaletteElements <- IO.pure(
      model.palette.palettes.map(palette => stencilNavList(palette))
    )
    newConnectorsElements <- IO.pure(
      model.palette.palettes.map(palette => connectorNavList(palette))
    )
    _ <- IO {
      newPaletteElements.foreach(paletteContainer.appendChild)
      newConnectorsElements.foreach(paletteContainer.appendChild)
    }
    _ <- IO(paletteContainer.appendChild(closeButton))
  } yield ()
}
