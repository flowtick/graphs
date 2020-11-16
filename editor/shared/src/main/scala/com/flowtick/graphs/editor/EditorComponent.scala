package com.flowtick.graphs.editor

import cats.effect.IO

trait EditorComponent {
  type Eval = EditorContext => IO[EditorContext]
  type Transform = EditorContext => EditorContext

  def order: Double = 0.0

  def init(model: EditorModel): IO[Unit] = IO.unit

  def eval: Eval
}
