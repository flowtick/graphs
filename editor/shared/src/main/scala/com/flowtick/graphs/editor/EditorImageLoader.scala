package com.flowtick.graphs.editor

import cats.effect.IO
import cats.implicits._
import com.flowtick.graphs.style.StyleSheetLike

class EditorImageLoader[Image](imageLoader: ImageLoader[Image]) extends EditorComponent {
  override def order: Double = 0.45

  override def init(model: EditorModel): IO[Unit] = {
    registerStyleSheetImages(model.styleSheet).void
  }

  override def eval: Eval = ctx =>
    ctx.effect(this) { case Reset =>
      registerStyleSheetImages(ctx.model.styleSheet).void
    }

  def registerStyleSheetImages(
      styleSheet: StyleSheetLike
  ): IO[List[Either[Throwable, Image]]] =
    styleSheet.images
      .map { case (key, imageSpec) =>
        imageLoader.registerImage(key, imageSpec).attempt
      }
      .toList
      .sequence
}
