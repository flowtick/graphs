package com.flowtick.graphs.editor

import cats.effect.IO
import cats.implicits.toFlatMapOps
import javafx.scene.input.MouseEvent
import scalafx.scene.Node
import scalafx.scene.layout.BorderPane

class EditorViewJavaFx(bus: EditorMessageBus, layout: BorderPane)
    extends EditorView[Node, MouseEvent] {
  override def createPage: IO[Page[Node, MouseEvent]] = IO(
    new EditorGraphPane(layout)(handleSelect, handleDrag, handleDoubleClick)
  ).flatTap(pane => IO(layout.setCenter(pane)))

  override def messageBus: EditorMessageBus = bus
}
