package com.flowtick.graphs.editor

import cats.effect.IO
import org.scalajs.dom.raw.{Element, Event}

class EditorViewJs(containerElementId: String)(val messageBus: EditorMessageBus) extends EditorView[Element, Event] {
  lazy val container = org.scalajs.dom.window.document.getElementById(containerElementId)

  def createPage: IO[Page[Element, Event]] = for {
    newPage <- IO.pure(EditorSVGPageJs(handleSelect, handleDrag, handleDoubleClick)(
      EditorRendererJs.renderer,
      EditorDomEventLike
    ))
    currentPage <- pageRef.get
    _ <- currentPage match {
      case None => IO(container.insertBefore(newPage.root, container.firstChild))
      case Some(existing) => IO(container.replaceChild(newPage.root, existing.root))
    }
  } yield newPage
}
