package com.flowtick.graphs.view

import cats.effect.IO

import com.flowtick.graphs.layout.PointSpec
import com.flowtick.graphs.{Edge, Node}

sealed trait EventData

trait MouseEventLike extends EventData {
  def clientX: Double
  def clientY: Double
}

final case class EditorMouseEvent(clientX: Double, clientY: Double, button: Int)
    extends MouseEventLike
final case class EditorWheelEvent(
    clientX: Double,
    clientY: Double,
    deltaY: Double
) extends MouseEventLike

// TODO remove editor prefix
case object EditorAnyEvent extends EventData

trait EventLike[E, T] {
  def target(event: E): T
  def preventDefault(event: E): Unit
  def data(event: E): EventData
}

class SVGPage[
    Builder,
    Output <: Frag,
    Frag,
    EventType,
    MatrixType,
    NodeType,
    EdgeType,
    Model <: ViewContextLike[
      EdgeType,
      NodeType
    ]
](
    renderer: SVGRenderer[Builder, Output, Frag, MatrixType],
    eventLike: EventLike[EventType, Output]
) extends Page[Output, EventType, NodeType, EdgeType, Model] {
  def clientWidth: Double = 500
  def clientHeight: Double = 500
  def scrollSpeed: Double = 1.0

  var panStart: Option[PanContext] = None

  def startPan(event: EventType): Boolean = eventLike.data(event) match {
    case mouseEvent: EditorMouseEvent =>
      if (panStart.isEmpty && mouseEvent.button == 2) {
        val cursor = screenCoordinates(mouseEvent.clientX, mouseEvent.clientY)
        val offset = renderer.getPageOffset
        panStart = Some(PanContext(cursor.x, cursor.y, offset.x, offset.y))
      }
      false
    case _ => false
  }

  def stopPan(event: EventType): IO[Unit] = IO {
    panStart = None
  }

  def screenCoordinates(x: Double, y: Double): Point =
    renderer.screenCoordinates(x, y)

  def pageCoordinates(x: Double, y: Double): Point =
    renderer.pageCoordinates(x, y)

  def pageCenter: PointSpec = {
    val coordinates = pageCoordinates(clientWidth / 2.0, clientHeight / 2.0)
    PointSpec(coordinates.x, coordinates.y)
  }

  def pan(evt: EventType): Unit =
    panStart match {
      case Some(start) =>
        eventLike.data(evt) match {
          case mouseEvent: EditorMouseEvent =>
            val cursor =
              screenCoordinates(mouseEvent.clientX, mouseEvent.clientY)

            val dx = cursor.x - start.mouseAnchorX
            val dy = cursor.y - start.mouseAnchorY

            renderer.setViewPortOffset(
              start.translateAnchorX + dx,
              start.translateAnchorY + dy
            )
          case _ =>
        }
      case None =>
    }

  def zoom(evt: EventType): Unit = eventLike.data(evt) match {
    case wheelEvent: EditorWheelEvent =>
      val cursor = pageCoordinates(wheelEvent.clientX, wheelEvent.clientY)

      val scaleDelta = wheelEvent.deltaY * scrollSpeed
      val zoom = 1 + scaleDelta

      renderer.translateAndScaleView(
        -(cursor.x * (zoom - 1)),
        -(cursor.y * (zoom - 1)),
        zoom
      )
    case _ =>
  }

  def click: EventType => IO[Option[ElementRef]] = evt =>
    IO {
      val elem = eventLike.target(evt)
      renderer.selectable(elem)
    }

  def startDrag: EventType => IO[Option[DragStart[Output]]] = evt =>
    for {
      dragStart <- IO {
        val elem = eventLike.target(evt)

        eventLike.data(evt) match {
          case mouse: EditorMouseEvent =>
            renderer.draggable(elem).map { elementRef =>
              val startCursor = pageCoordinates(mouse.clientX, mouse.clientY)
              val pageTransform =
                pageCoordinates(renderer.x(elem), renderer.y(elem))
              DragStart[Output](
                startCursor.x,
                startCursor.y,
                pageTransform.x,
                pageTransform.y,
                elem,
                elementRef,
                None,
                0.0,
                0.0
              )
            }
          case _ => None
        }
      }
      _ <- dragStartRef.set(dragStart)
    } yield dragStart

  override def addEdge(
      edge: Edge[EdgeType],
      model: Model
  ): IO[Option[GraphElement[Output]]] = for {
    edgeElement <- renderer.renderEdge(
      edge,
      model
    )
  } yield edgeElement

  override def addNode(
      node: Node[NodeType],
      model: Model
  ): IO[Option[GraphElement[Output]]] = for {
    node <- renderer.renderNode(node, model)
  } yield Some(node)

  override def setSelection(element: GraphElement[Output]): IO[Unit] =
    renderer.selectElement(element: GraphElement[Output])

  override def unsetSelection(element: GraphElement[Output]): IO[Unit] =
    renderer.unselectElement(element)

  override def deleteElement(element: GraphElement[Output]): IO[Unit] =
    renderer.deleteElement(element)

  override def resetTransformation: IO[Unit] =
    renderer.resetMatrix

  override def beforeDrag: EventType => IO[Unit] = e => IO(eventLike.preventDefault(e))

  override def eventCoordinates(event: EventType): Point = eventLike.data(event) match {
    case e: EditorMouseEvent => PagePoint(e.clientX, e.clientY)
    case _                   => PagePoint(0, 0)
  }

  override def applyDrag: DragStart[Output] => IO[Unit] = dragStart =>
    dragStart.lastPos match {
      case Some(pos) => IO(renderer.setPosition(dragStart.dragElem)(pos.x, pos.y))
      case _         => IO.unit
    }

  override def root: Output = renderer.graphSVG.root
}
