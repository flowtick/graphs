package com.flowtick.graphs.editor

import cats.effect.IO
import com.flowtick.graphs._
import org.scalajs.dom.raw.SVGElement

class EditorViewJs(containerElementId: String)(val messageBus: EditorMessageBus) extends EditorView[SVGElement] {

  lazy val container = org.scalajs.dom.window.document.getElementById(containerElementId)

  def createPage: IO[Page[SVGElement]] = IO(SVGPage(handleSelect, handleDrag, handleDoubleClick))

  override def init(model: EditorModel): IO[Unit] = page.get.map(p => container.insertBefore(p.root, container.firstChild))

  override def setNewPage(newPage: Page[SVGElement], oldPage: Page[SVGElement]): IO[Unit] =
    IO(container.replaceChild(newPage.root, oldPage.root))
}
