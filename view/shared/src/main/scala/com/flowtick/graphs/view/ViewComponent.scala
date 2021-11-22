package com.flowtick.graphs.view

import cats.effect.IO

trait ViewComponent[Context, Model] {
  type Eval = Context => IO[Context]
  type Transform = Context => Context

  // TODO: an explicit dependency approach would be less error prone
  def order: Double = 0.0

  def init(model: Model): IO[Unit] = IO.unit

  def eval: Eval
}
