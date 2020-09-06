package com.flowtick.graphs.editor

import java.util.UUID

import cats.effect.IO
import cats.effect.concurrent.Ref
import com.flowtick.graphs._
import org.scalajs.dom.html.{Button, Div, UList}
import org.scalajs.dom.raw.{Event, HTMLElement}
import scalatags.JsDom.all._

class EditorPaletteJs(paletteElementId: String)(messageBus: EditorMessageBus) extends EditorComponent {
  val currentStencilItemRef: Ref[IO, Option[Stencil]] = Ref.unsafe(None)
  val currentConnectorItemRef: Ref[IO, Option[Connector]] = Ref.unsafe(None)

  lazy val paletteContainer: HTMLElement = org.scalajs.dom.window.document
    .getElementById(paletteElementId)
    .asInstanceOf[HTMLElement]

  def stencilNavList(palette: Palette): UList = ul(
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
          imageOrTitle(palette.images, item.previewImageRef, item.title)
        )
      ))
    ))).render

  def connectorNavList(palette: Palette): UList = ul(
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
          imageOrTitle(palette.images, item.previewImageRef, item.title)
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

  def selectPaletteItem(paletteItem: Stencil): Unit = {
    currentStencilItemRef.set(Some(paletteItem)).attempt.unsafeRunSync()
  }

  def selectConnectorItem(connectorItem: Connector): Unit = {
    currentConnectorItemRef.set(Some(connectorItem)).attempt.unsafeRunSync()
  }

  def createPaletteItem(paletteItem: Stencil): Unit = {
    messageBus
      .publish(CreateNode(UUID.randomUUID().toString, Some(paletteItem.id)))
      .attempt
      .unsafeRunSync()
  }

  override def eval: Eval = ctx => ctx.effect(this) {
    case Toggle(Toggle.paletteKey, enabled) => IO(toggleView(enabled))
  }.flatMap(_.transformIO {
    case create: CreateNode =>
      for {
        current <- currentStencilItemRef.get
      } yield current match {
        case Some(item) => ctx.copy(event = create.copy(stencilRef = Some(item.id)))
        case None => ctx
      }
    case addEdge: AddEdge =>
      for {
        current <- currentConnectorItemRef.get
      } yield current match {
        case Some(item) =>
          println(current)
          ctx.copy(event = addEdge.copy(stencilRef = Some(item.id)))
        case None => ctx
      }
  })

  def toggleView(enabled: Boolean): Unit = {
    if (enabled) {
      paletteContainer.classList.remove("d-none")
    } else {
      paletteContainer.classList.add("d-none")
    }
  }

  lazy val closeButton: Button =
    button(
      cls := "close float-right",
      `type` := "button",
      data("dismiss") := "modal",
      aria.label := "Close",
      span(aria.hidden := "true", "×"),
      onclick := ((_: Event) => messageBus.publish(Toggle(Toggle.paletteKey, value = false)).unsafeRunSync())
    ).render

  override def init(model: EditorModel): IO[Unit] = for {
    newPaletteElement <- IO.pure(stencilNavList(model.palette))
    newConnectorsElement <- IO.pure(connectorNavList(model.palette))
    _ <- IO(toggleView(false))
    _ <- IO {
      paletteContainer.appendChild(newPaletteElement)
      paletteContainer.appendChild(newConnectorsElement)
    }
    _ <- IO(paletteContainer.appendChild(closeButton))
  } yield ()
}
