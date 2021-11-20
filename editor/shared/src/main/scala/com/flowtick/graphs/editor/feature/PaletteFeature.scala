package com.flowtick.graphs.editor.feature

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.flowtick.graphs.editor._

import java.util.UUID
import cats.effect.kernel.Ref
import com.flowtick.graphs.view.ViewComponent

trait PaletteFeature extends EditorComponent {
  def messageBus: EditorMessageBus
  def toggleView(enabled: Boolean): IO[Boolean]
  def initPalette(model: EditorModel): IO[Unit]

  override def order: Double = 0.5

  val currentStencilItemRef: Ref[IO, Option[Stencil]] = Ref.unsafe(None)
  val currentConnectorItemRef: Ref[IO, Option[Connector]] = Ref.unsafe(None)
  val visibleRef: Ref[IO, Option[Boolean]] = Ref.unsafe(None)

  override def eval: Eval = ctx =>
    ctx
      .effect(this) { case EditorToggle(EditorToggle.paletteKey, enabled) =>
        val show = if (enabled.isEmpty) {
          visibleRef.get
            .flatMap(visible => toggleView(visible.forall(state => !state)))
        } else toggleView(enabled.getOrElse(true))

        show.flatMap(visible => visibleRef.set(Some(visible)))
      }
      .flatMap(_.transformIO {
        case addNode: AddNode =>
          for {
            current <- currentStencilItemRef.get
          } yield current match {
            case Some(item) =>
              ctx.copy(event = addNode.copy(stencilRef = Some(item.id)))
            case None => ctx
          }
        case addEdge: AddEdge =>
          for {
            current <- currentConnectorItemRef.get
          } yield current match {
            case Some(item) =>
              ctx.copy(event = addEdge.copy(connectorRef = Some(item.id)))
            case None => ctx
          }
      })

  override def init(model: EditorModel): IO[Unit] = for {
    _ <- IO(toggleView(false))
    _ <- initPalette(model)
  } yield ()

  def selectPaletteItem(paletteItem: Stencil): Unit = {
    currentStencilItemRef.set(Some(paletteItem)).attempt.unsafeToFuture()
  }

  def selectConnectorItem(connectorItem: Connector): Unit = {
    currentConnectorItemRef.set(Some(connectorItem)).attempt.unsafeToFuture()
  }

  def createFromStencil(paletteItem: Stencil): Unit = {
    messageBus
      .publish(AddNode(UUID.randomUUID().toString, Some(paletteItem.id)))
      .attempt
      .unsafeToFuture()
  }
}
