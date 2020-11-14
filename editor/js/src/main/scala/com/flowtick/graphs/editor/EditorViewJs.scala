package com.flowtick.graphs.editor

import cats.effect.IO
import org.scalajs.dom.raw.{Event, SVGElement}

class EditorViewJs(containerElementId: String)(val messageBus: EditorMessageBus) extends EditorView[SVGElement, Event] {

  lazy val container = org.scalajs.dom.window.document.getElementById(containerElementId)

  def createPage: IO[Page[SVGElement, Event]] = IO(SVGPage(handleSelect, handleDrag, handleDoubleClick))

  override def init(model: EditorModel): IO[Unit] = page.get.map(p => container.insertBefore(p.root, container.firstChild))

  override def setNewPage(newPage: Page[SVGElement, Event], oldPage: Page[SVGElement, Event]): IO[Unit] =
    IO(container.replaceChild(newPage.root, oldPage.root))
}
