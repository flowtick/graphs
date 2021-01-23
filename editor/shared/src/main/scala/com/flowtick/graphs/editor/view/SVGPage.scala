package com.flowtick.graphs.editor.view

import cats.effect.IO
import com.flowtick.graphs.editor._
import com.flowtick.graphs.layout.PointSpec
import com.flowtick.graphs.{Edge, Node}

sealed trait EventData

trait MouseEventLike extends EventData {
  def clientX: Double
  def clientY: Double
}

final case class EditorMouseEvent(clientX: Double, clientY: Double, button: Int) extends MouseEventLike
final case class EditorWheelEvent(clientX: Double, clientY: Double, deltaY: Double) extends MouseEventLike
case object EditorAnyEvent extends EventData

trait EventLike[E, T] {
  def target(event: E): T
  def preventDefault(event: E): Unit
  def data(event: E): EventData
}

class SVGPage[Builder, T <: Frag, Frag, E, M](renderer: SVGRenderer[Builder, T, Frag, M], eventLike: EventLike[E, T]) extends Page[T, E] {
  var panStart: Option[PanContext] = None

  def startPan(event: E): Boolean = eventLike.data(event) match {
    case mouseEvent: EditorMouseEvent =>
      if (panStart.isEmpty && mouseEvent.button == 2) {
        val cursor = screenCoordinates(mouseEvent.clientX, mouseEvent.clientY)
        val offset = renderer.getPageOffset
        panStart = Some(PanContext(cursor.x, cursor.y, offset.x, offset.y))
      }
      false
    case _ => false
  }

  def stopPan(event: E): Unit = {
    panStart = None
  }

  def screenCoordinates(x: Double, y: Double): Point = renderer.screenCoordinates(x, y)

  def pageCoordinates(x: Double, y: Double): Point = renderer.pageCoordinates(x, y)

  def pageCenter: PointSpec = {
    val coordinates = pageCoordinates(renderer.clientWidth / 2.0, renderer.clientHeight / 2.0)
    PointSpec(coordinates.x, coordinates.y)
  }

  def pan(evt: E): Unit =
    panStart match {
      case Some(start) =>
        eventLike.data(evt) match {
          case mouseEvent: EditorMouseEvent =>
            val cursor = screenCoordinates(mouseEvent.clientX, mouseEvent.clientY)

            val dx = cursor.x - start.mouseAnchorX
            val dy = cursor.y - start.mouseAnchorY

            renderer.setViewPortOffset(start.translateAnchorX + dx, start.translateAnchorY + dy)
          case _ =>
        }
      case None =>
    }

  def zoom(evt: E): Unit = eventLike.data(evt) match {
    case wheelEvent: EditorWheelEvent =>
      val cursor = pageCoordinates(wheelEvent.clientX, wheelEvent.clientY)

      val scaleDelta = wheelEvent.deltaY * renderer.scrollSpeed
      val zoom = 1 + scaleDelta

      renderer.translateAndScaleView(-(cursor.x*(zoom-1)),-(cursor.y*(zoom-1)), zoom)
    case _ =>
  }

  def click: E => Option[ElementRef] = evt =>
    IO {
      val elem = eventLike.target(evt)
      renderer.selectable(elem)
    }.unsafeRunSync()

  def startDrag: E => Option[DragStart[T]] = evt => (for {
    dragStart <- IO {
      val elem = eventLike.target(evt)

      eventLike.data(evt) match {
        case mouse: EditorMouseEvent =>
          renderer.draggable(elem).map { elementRef =>
            val startCursor = pageCoordinates(mouse.clientX, mouse.clientY)
            val pageTransform = pageCoordinates(renderer.x(elem), renderer.y(elem))
            DragStart[T](startCursor.x, startCursor.y, pageTransform.x, pageTransform.y, elem, elementRef, None, 0.0, 0.0)
          }
        case _ => None
      }
    }
    _ <- dragStartRef.set(dragStart)
  } yield dragStart).unsafeRunSync()

  override def addEdge(edge: Edge[EditorGraphEdge], model: EditorModel): IO[Option[GraphElement[T]]] = for {
    edgeElement <- renderer.renderEdge(edge, model.graph, model.layout, model.styleSheet)
  } yield edgeElement

  override def addNode(node: Node[EditorGraphNode], model: EditorModel): IO[Option[GraphElement[T]]] = for {
    node <- renderer.renderNode(node, model.layout, model.styleSheet)
  } yield Some(node)

  override def setSelection(element: GraphElement[T]): IO[Unit] =
    renderer.selectElement(element: GraphElement[T])

  override def unsetSelection(element: GraphElement[T]): IO[Unit] =
    renderer.unselectElement(element)

  override def deleteElement(element: GraphElement[T]): IO[Unit] =
    renderer.deleteElement(element)

  override def resetTransformation: IO[Unit] =
    renderer.resetMatrix

  override def beforeDrag: E => Unit = e => eventLike.preventDefault(e)

  override def eventCoordinates(event: E): Point = eventLike.data(event) match {
    case e: EditorMouseEvent => PagePoint(e.clientX, e.clientY)
    case _ => PagePoint(0, 0)
  }

  override def applyDrag: DragStart[T] => Unit = dragStart => dragStart.lastPos match {
    case Some(pos) => renderer.setPosition(dragStart.dragElem)(pos.x, pos.y)
    case _ =>
  }

  override def root: T = renderer.graphSVG.root
}
