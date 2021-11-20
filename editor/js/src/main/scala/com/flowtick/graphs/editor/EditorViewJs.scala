package com.flowtick.graphs.editor

import cats.effect.IO
import com.flowtick.graphs.editor.view.SVGRendererJs
import com.flowtick.graphs.view.SVGRendererOptions
import org.scalajs.dom.raw.{Element, Event}

class EditorViewJs(containerElementId: String)(messageBus: EditorMessageBus)
    extends EditorView[Element, Event](messageBus) {
  lazy val container =
    org.scalajs.dom.window.document.getElementById(containerElementId)

  def createPage: IO[PageType] = for {
    newPage <- IO.pure(
      EditorPageJs(handleSelect, handleDrag, handleDoubleClick)(
        SVGRendererJs(SVGRendererOptions(showOrigin = true)),
        EditorDomEventLike
      )
    )
    currentPage <- pageRef.get
    _ <- currentPage match {
      case None =>
        IO(container.insertBefore(newPage.root, container.firstChild))
      case Some(existing) =>
        IO(container.replaceChild(newPage.root, existing.root))
    }
  } yield newPage
}
