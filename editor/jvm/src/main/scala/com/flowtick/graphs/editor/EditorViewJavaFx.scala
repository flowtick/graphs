package com.flowtick.graphs.editor

import cats.effect.IO
import scalafx.scene.Node
import scalafx.scene.layout.BorderPane

class EditorViewJavaFx(bus: EditorMessageBus, layout: BorderPane) extends EditorView[Node] {
  override def init(model: EditorModel): IO[Unit] = IO.unit

  override def createPage: IO[Page[Node]] = IO(new EditorGraphPane(handleSelect, handleDrag, handleDoubleClick))

  override def messageBus: EditorMessageBus = bus

  override def setNewPage(newPage: Page[Node], oldPage: Page[Node]): IO[Unit] = IO {
    layout.setCenter(newPage.root)
  }
}