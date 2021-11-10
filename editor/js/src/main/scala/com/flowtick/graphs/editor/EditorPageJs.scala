package com.flowtick.graphs.editor

import cats.effect.IO
import com.flowtick.graphs.editor.view._
import org.scalajs.dom
import org.scalajs.dom.raw._
import cats.effect.unsafe.implicits.global

object EditorDomEventLike extends EventLike[Event, dom.Element] {
  override def target(event: Event): dom.Element =
    event.target.asInstanceOf[Element]
  override def preventDefault(event: Event): Unit = event.preventDefault()
  override def data(event: Event): EventData = {
    event match {
      case wheel: WheelEvent =>
        EditorWheelEvent(wheel.clientX, wheel.clientY, wheel.deltaY)
      case mouseEvent: MouseEvent =>
        EditorMouseEvent(
          mouseEvent.clientX,
          mouseEvent.clientY,
          mouseEvent.button
        )
      case _ => EditorAnyEvent
    }
  }
}

object EditorPageJs {
  def apply(
      handleSelect: ElementRef => Boolean => IO[Unit],
      handleDrag: Option[DragStart[dom.Element]] => IO[Unit],
      handleDoubleClick: Event => IO[Unit]
  )(
      renderer: SVGRenderer[dom.Element, dom.Element, dom.Node, SVGMatrix],
      eventLike: EventLike[Event, dom.Element]
  ): Page[dom.Element, Event] = {
    val page =
      new SVGPage[dom.Element, dom.Element, dom.Node, Event, SVGMatrix](
        renderer,
        eventLike
      ) {
        override def clientWidth: Double = renderer.graphSVG.root.clientWidth
        override def clientHeight: Double = renderer.graphSVG.root.clientHeight
        override def scrollSpeed: Double = if (dom.window.navigator.userAgent.contains("Firefox"))
          0.03
        else 0.003
      }

    renderer.graphSVG.panZoomRect.foreach(
      _.addEventListener(
        "mousedown",
        (e: MouseEvent) => {
          page.startPan(e)
        }
      )
    )

    renderer.graphSVG.panZoomRect.foreach(
      _.addEventListener("mouseup", e => page.stopPan(e))
    )
    renderer.graphSVG.panZoomRect.foreach(
      _.addEventListener(
        "mousemove",
        (e: MouseEvent) => {
          page.pan(e)
        }
      )
    )

    renderer.graphSVG.root.addEventListener("wheel", page.zoom _)

    renderer.graphSVG.root.addEventListener(
      "mousedown",
      (e: MouseEvent) => {
        page.startDrag(e).flatMap {
          case Some(_) => IO.unit // we already have a selection
          case None =>
            page.click(e).flatMap {
              case Some(clicked) => handleSelect(clicked)(e.ctrlKey)
              case None          => IO.unit
            }
        }
      }
    )

    renderer.graphSVG.root.addEventListener("mousemove", page.drag)
    renderer.graphSVG.root.addEventListener(
      "mouseup",
      (e: MouseEvent) =>
        {
          for {
            drag <- page.endDrag(e)
            _ <- handleDrag(drag).attempt
            result <- drag match {
              case Some(drag) if Math.abs(drag.deltaX) < 2 && Math.abs(drag.deltaY) < 2 =>
                page.click(e).flatMap {
                  case Some(element) => handleSelect(element)(false)
                  case None          => IO.unit
                }
              case _ => IO.unit
            }
          } yield result
        }.unsafeToFuture()
    )

    renderer.graphSVG.root.addEventListener(
      "mouseleave",
      (e: MouseEvent) =>
        {
          for {
            _ <- page.stopPan(e)
            drag <- page.endDrag(e)
            result <- handleDrag(drag)
          } yield result
        }.unsafeToFuture()
    )

    renderer.graphSVG.root.addEventListener(
      "dblclick",
      (e: MouseEvent) => {
        handleDoubleClick(e).unsafeToFuture()
      }
    )

    page
  }
}
