package com.flowtick.graphs.editor

import java.util.UUID

import cats.effect.IO
import cats.effect.concurrent.Ref

trait EditorPalette extends EditorComponent {
  def messageBus: EditorMessageBus
  def toggleView(enabled: Boolean): IO[Boolean]
  def initPalette(model: EditorModel): IO[Unit]

  override def order: Double = 0.5

  val currentStencilItemRef: Ref[IO, Option[Stencil]] = Ref.unsafe(None)
  val currentConnectorItemRef: Ref[IO, Option[Connector]] = Ref.unsafe(None)
  val visibleRef: Ref[IO, Option[Boolean]] = Ref.unsafe(None)

  override def eval: Eval = ctx => ctx.effect(this) {
    case Toggle(Toggle.paletteKey, enabled) =>
      val show = if (enabled.isEmpty) {
        visibleRef
          .get
          .flatMap(visible => toggleView(visible.forall(state => !state)))
      } else toggleView(enabled.getOrElse(true))

      show.flatMap(visible => visibleRef.set(Some(visible)))
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
          ctx.copy(event = addEdge.copy(stencilRef = Some(item.id)))
        case None => ctx
      }
  })

  override def init(model: EditorModel): IO[Unit] = for {
    _ <- IO(toggleView(false))
    _ <- initPalette(model)
  } yield ()

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
}
