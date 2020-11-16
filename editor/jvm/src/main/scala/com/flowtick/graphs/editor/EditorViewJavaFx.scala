package com.flowtick.graphs.editor

import cats.effect.IO
import javafx.scene.input.MouseEvent
import scalafx.scene.Node
import scalafx.scene.layout.BorderPane

class EditorViewJavaFx(bus: EditorMessageBus, layout: BorderPane) extends EditorView[Node, MouseEvent] {
  override def init(model: EditorModel): IO[Unit] = IO.unit

  override def createPage: IO[Page[Node, MouseEvent]] = IO(new EditorGraphPane(layout)(handleSelect, handleDrag, handleDoubleClick))

  override def messageBus: EditorMessageBus = bus

  override def setNewPage(newPage: Page[Node, MouseEvent], oldPage: Page[Node, MouseEvent]): IO[Unit] = IO {
    layout.setCenter(newPage.root)
  }
}