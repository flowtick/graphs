package com.flowtick.graphs

import cats.effect.IO

trait EditorComponent {
  type Eval = EditorContext => IO[EditorContext]
  type Transform = EditorContext => EditorContext

  def init(model: EditorModel): IO[Unit] = IO.unit

  def eval: Eval
}
