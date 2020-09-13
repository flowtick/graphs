package com.flowtick.graphs.editor

import cats.effect.IO
import cats.effect.concurrent.Ref
import com.flowtick.graphs.graphml.{GraphMLEdge, GraphMLGraph, GraphMLNode, PointSpec}
import com.flowtick.graphs.{Edge, EdgeType, ElementRef, GraphElement, Node, NodeType, Page}
import io.circe.Json
import org.scalajs.dom
import org.scalajs.dom.raw.{MouseEvent, SVGElement, SVGMatrix, SVGPoint, WheelEvent}
import org.scalajs.dom.svg.{G, RectElement, SVG}
import scalatags.JsDom.all.{id, _}
import scalatags.JsDom.{svgAttrs, svgTags => svg}

final case class PanStart(cursorX: Double, cursorY: Double, transformX: Double, transformY: Double)

final case class DragStart(cursorX: Double,
                           cursorY: Double,
                           transform: SVGMatrix,
                           dragElem: SVGElement,
                           element: ElementRef,
                           lastPos: Option[(Double, Double)])

class SVGPage(val root: SVG,
              val viewPort: G,
              val edgeGroup: G,
              val nodeGroup: G,
              val selectGroup: G,
              val labelGroup: G,
              val panZoomRect: RectElement,
              var pageMatrix: SVGMatrix) extends Page[SVGElement] {
  type Point = SVGPoint

  var dragStartRef: Ref[IO, Option[DragStart]] = Ref.unsafe(None)
  var panStart: Option[PanStart] = None

  def tx: Double = pageMatrix.e
  def tx_= (value: Double): Unit = pageMatrix.e = value

  def ty: Double = pageMatrix.f
  def ty_= (value: Double): Unit = pageMatrix.f = value

  def scalex: Double = pageMatrix.a
  def scalex_= (value: Double): Unit = pageMatrix.a = value

  def scaley: Double = pageMatrix.d
  def scaley_= (value: Double): Unit = pageMatrix.d = value

  def startPan(mouseEvent: MouseEvent): Unit = {
    if (panStart.isEmpty) {
      val cursor = screenCoordinates(mouseEvent.clientX, mouseEvent.clientY)
      panStart = Some(PanStart(cursor.x, cursor.y, tx, ty))
    }
  }

  def stopPan(mouseEvent: MouseEvent): Unit = {
    panStart = None
  }

  def screenCoordinates(x: Double, y: Double): Point = {
    val pt = root.createSVGPoint()
    pt.x = x
    pt.y = y
    pt.matrixTransform(root.getScreenCTM.inverse)
  }

  def pageCoordinates(x: Double, y: Double): Point = {
    val pt = root.createSVGPoint()
    pt.x = x
    pt.y = y
    pt.matrixTransform(pageMatrix.inverse())
  }

  def pageCenter: PointSpec = {
    val coordinates = pageCoordinates(root.clientWidth / 2.0, root.clientHeight / 2.0)
    PointSpec(coordinates.x, coordinates.y)
  }

  def pan(evt: MouseEvent): Unit =
    panStart match {
      case Some(start) =>
      val cursor = screenCoordinates(evt.clientX, evt.clientY)

      val dx = cursor.x - start.cursorX
      val dy = cursor.y - start.cursorY

      tx = start.transformX + dx
      ty = start.transformY + dy

      SVGUtil.setMatrix(viewPort, pageMatrix)

      case None =>
    }

  def zoom(evt: WheelEvent): Unit = {
    evt.preventDefault()

    val cursor = pageCoordinates(evt.clientX, evt.clientY)

    val scaleDelta = evt.deltaY * SVGPage.scrollSpeed
    val zoom = 1 + scaleDelta

    pageMatrix = pageMatrix
      .translate(-(cursor.x*(zoom-1)),-(cursor.y*(zoom-1)))
      .scale(zoom)

    SVGUtil.setMatrix(viewPort, pageMatrix)
  }

  def referenceFromElement(elem: SVGElement): ElementRef = {
    val elementType = elem.getAttribute("data-type")
    val elementId = elem.getAttribute("data-id")

    val refType = elementType match {
      case "node" => NodeType
      case "edge" => EdgeType
    }

    ElementRef(elementId, refType)
  }

  def click: MouseEvent => Option[ElementRef] = evt =>
    IO {
      val elem = evt.target.asInstanceOf[SVGElement]
      if (elem.classList.contains("selectable")) {
        Some(referenceFromElement(elem))
      } else None
    }.unsafeRunSync()

  def startDrag: MouseEvent => Option[DragStart] = evt => (for {
    dragStart <- IO {
      val elem = evt.target.asInstanceOf[SVGElement]
      if (elem.classList.contains("draggable")) {
        val startCursor = screenCoordinates(evt.clientX, evt.clientY)
        val selectElement = elem.asInstanceOf[RectElement]
        val elementRef = referenceFromElement(selectElement)

        Some(DragStart(startCursor.x, startCursor.y, selectElement.getCTM(), selectElement, elementRef, None))
      } else None
    }
    _ <- dragStartRef.set(dragStart)
  } yield dragStart).unsafeRunSync()

  def drag: MouseEvent => Unit = evt => dragStartRef.update {
    case Some(dragStart: DragStart) =>
      evt.preventDefault()
      val dragCursor = screenCoordinates(evt.clientX, evt.clientY)

      val gridSize: Int = 10

      val deltaX = dragCursor.x - dragStart.cursorX
      val deltaY = dragCursor.y - dragStart.cursorY

      val point = pageCoordinates(dragStart.transform.e + deltaX, dragStart.transform.f + deltaY)

      val tx = (point.x.toInt / gridSize) * gridSize
      val ty = (point.y.toInt / gridSize) * gridSize

      SVGUtil.setTransform(dragStart.dragElem, s"translate($tx $ty)")

      Some(dragStart.copy(lastPos = Some(tx, ty)))
    case None => None
  }.unsafeRunSync()

  def endDrag: MouseEvent => Option[DragStart] = _ => dragStartRef.getAndUpdate(_ => None).unsafeRunSync()

  override def addEdge(edge: Edge[GraphMLEdge[Json]], graphml: GraphMLGraph[Json, Json]): IO[Option[GraphElement[SVGElement]]] = for {
    edgeElement <- IO(SVGGraphRenderer.renderEdge(edge, graphml))
    _ <- IO(edgeElement.foreach(edge => {
      edgeGroup.appendChild(edge.group)
      labelGroup.appendChild(edge.label)
    }))
  } yield edgeElement

  override def addNode(node: Node[GraphMLNode[Json]], graphml: GraphMLGraph[Json, Json]): IO[Option[GraphElement[SVGElement]]] = for {
    node <- IO(node.value.shape.map { shape =>
      val nodeElement = SVGGraphRenderer.renderNode(node.id, shape, graphml.resourcesById)
      nodeGroup.appendChild(nodeElement.group)
      selectGroup.appendChild(nodeElement.selectElem)

      nodeElement
    })
  } yield node
}

object SVGPage {
  lazy val scrollSpeed: Double = if (dom.window.navigator.userAgent.contains("Firefox")) 0.03 else 0.003

  def apply(): SVGPage = {
    val panZoomRect = svg.rect(
      id := "pan-zoom-hit",
      svgAttrs.width := "100%",
      svgAttrs.height := "100%",
      svgAttrs.fill := "none",
      svgAttrs.style := "pointer-events:all"
    ).render

    val edges = svg.g(id := "edges").render
    val nodes = svg.g(id := "nodes").render
    val select = svg.g(id := "select").render
    val label = svg.g(id := "label").render

    val originStroke = "#CCC"
    val originStrokeWidth = 2
    val originDim = 5
    val originMarker = svg.g(
      id := "origin",
      svg.line(
        svgAttrs.strokeWidth := originStrokeWidth,
        svgAttrs.x1 := -originDim,
        svgAttrs.x2 := originDim,
        svgAttrs.y1 := 0,
        svgAttrs.y2 := 0,
        svgAttrs.stroke := originStroke
      ),
      svg.line(
        svgAttrs.strokeWidth := originStrokeWidth,
        svgAttrs.y1 := -originDim,
        svgAttrs.y2 := originDim,
        svgAttrs.x1 := 0,
        svgAttrs.x2 := 0,
        svgAttrs.stroke := originStroke
      ),
    )

    val viewPort = svg.g(
      id := "viewport",
      svgAttrs.transform := "matrix(1 0 0 1 0 0)",
      originMarker,
      edges,
      nodes,
      select,
      label
    ).render

    val svgElem = svg.svg(
      svg.defs(
        svg.marker(
          svgAttrs.id := "arrow_standard",
          svgAttrs.viewBox := "0 0 10 10",
          svgAttrs.refX := "10",
          svgAttrs.refY := "5",
          svgAttrs.markerWidth := "10",
          svgAttrs.markerHeight := "10",
          svgAttrs.orient := "auto-start-reverse",
          svg.path(svgAttrs.d := "M 0 0 L 10 5 L 0 10 z")
        ),
        svg.marker(
          svgAttrs.id := "arrow_circle",
          svgAttrs.viewBox := "0 0 10 10",
          svgAttrs.refX := "5",
          svgAttrs.refY := "5",
          svgAttrs.markerWidth := "8",
          svgAttrs.markerHeight := "8",
          svg.circle(
            svgAttrs.cx := 5,
            svgAttrs.cy := 5,
            svgAttrs.r := 5,
            svgAttrs.fill := "black"
          )
        )
      ),
      svgAttrs.width := "100%",
      svgAttrs.height := "100%",
      svgAttrs.style := "border: 1px solid #ccc",
      svgAttrs.preserveAspectRatio := "none",
      panZoomRect,
      viewPort
    ).render

    new SVGPage(svgElem, viewPort, edges, nodes, select, label, panZoomRect, svgElem.createSVGMatrix())
  }
}
