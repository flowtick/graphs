package com.flowtick.graphs.editor

import cats.effect.IO
import com.flowtick.graphs
import com.flowtick.graphs.graphml.{GraphMLEdge, GraphMLGraph, GraphMLNode, PointSpec}
import com.flowtick.graphs._
import io.circe.Json
import javafx.event.EventHandler
import javafx.scene.input.{MouseEvent, ScrollEvent}
import scalafx.geometry.VPos
import scalafx.scene.effect.{Glow, InnerShadow}
import scalafx.scene.layout.{BorderPane, Pane}
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Line, Polygon, Polyline, Rectangle}
import scalafx.scene.text.{Text, TextAlignment}
import scalafx.scene.transform.Affine
import scalafx.scene.{Group, Node}

final case class JFXElement(id: String, group: Node, selectElem: Node, label : Node) extends GraphElement[Node]

class EditorGraphPane(handleSelect: ElementRef => IO[Unit]) extends BorderPane with Page[Node]{
  var dragContext = DragContext(0.0, 0.0, 0.0, 0.0)

  val transformation = new Affine()

  val group = new Pane {
    viewOrder_(1.0)
    children.add(createOriginCross)
    transforms.add(transformation)
  }

  val scaleDelta = 1.25

  center = group

  onScroll = new EventHandler[ScrollEvent]() {
    override def handle(event: ScrollEvent): Unit = {
      event.consume()

      if (event.getDeltaY != 0) {
        val scaleFactor = if (event.getDeltaY > 0) scaleDelta else 1 / scaleDelta

        val pivot = transformation.inverseTransform(event.getX, event.getY)
        transformation.appendScale(scaleFactor, scaleFactor, pivot.getX, pivot.getY)
      }
    }
  }

  onMousePressed = new EventHandler[MouseEvent] {
    override def handle(event: MouseEvent): Unit = {
      event.consume()

      if (!event.isPrimaryButtonDown) {
        dragContext = dragContext.copy(mouseAnchorX = event.getX)
        dragContext = dragContext.copy(mouseAnchorY = event.getY)

        dragContext = dragContext.copy(translateAnchorX = transformation.getTx)
        dragContext = dragContext.copy(translateAnchorY = transformation.getTy)
      }
    }
  }

  onMouseDragged = new EventHandler[MouseEvent] {
    override def handle(event: MouseEvent): Unit = {
      event.consume()

      if (!event.isPrimaryButtonDown) {
        val newX = dragContext.translateAnchorX + ((event.getX - dragContext.mouseAnchorX))
        val newY = dragContext.translateAnchorY + ((event.getY - dragContext.mouseAnchorY))

        transformation.setTx(newX)
        transformation.setTy(newY)
      }
    }
  }

  // draw origin cross
  def createOriginCross: Group = {
    new Group {
      children.add(new Line {
        startX = 5.0
        startY = 0.0
        endX = -5.0
        endY = 0.0
        stroke = Color.LightSlateGray
      })

      children.add(new Line {
        startX = 0.0
        startY = -5.0
        endX = 0.0
        endY = 5.0
        stroke = Color.LightSlateGray
      })
    }
  }

  override def root: Node = this

  override def pageCenter: PointSpec = {
    val center = transformation.inverseTransform(group.getWidth / 2, group.getHeight / 2)
    PointSpec(center.getX, center.getY)
  }

  def createArrowHead(fromx: Double, fromy: Double, tox: Double, toy: Double): Polygon = {
    val headlen = 10
    val dx = tox - fromx
    val dy = toy - fromy
    val angle = Math.atan2(dy, dx)
    new Polygon {
      points.addAll(tox - headlen * Math.cos(angle - Math.PI / 6), toy - headlen * Math.sin(angle - Math.PI / 6))
      points.addAll(tox, toy)
      points.addAll(tox - headlen * Math.cos(angle + Math.PI / 6), toy - headlen * Math.sin(angle + Math.PI / 6))
    }
  }

  override def addEdge(edge: Edge[GraphMLEdge[Json]], graphml: GraphMLGraph[Json, Json]): IO[Option[GraphElement[Node]]] = IO {
    println(s"adding edge...${edge}")

    for {
      edgePoints <- DrawUtil.getLinePoints(edge, graphml).map(_.toList.reverse)
      shape = edge.value.shape
      polyline = new Polyline {
        edgePoints.foreach(point => points.addAll(point.x, point.y))
        strokeWidth = 1.0
        stroke = Color.web("#000000")
      }
      last <- edgePoints.headOption
      secondLast <- edgePoints.tail.headOption

      arrowHead = createArrowHead(secondLast.x, secondLast.y, last.x, last.y)
    } yield {
      val textValue = shape.flatMap(_.label.map(_.text)).getOrElse("")

      val label = new Text() {
        text = textValue
        y = secondLast.y
        wrappingWidth = 12.0
        textAlignment = TextAlignment.Center
        textOrigin = VPos.Center
      }

      val edgeGroup = new Group {
        children.add(polyline)
        children.add(arrowHead)
      }

      group.children.add(edgeGroup)

      JFXElement(edge.id, edgeGroup, edgeGroup, label)
    }
  }

  override def addNode(node: graphs.Node[GraphMLNode[Json]], graphml: GraphMLGraph[Json, Json]): IO[Option[GraphElement[Node]]] = IO {
    for {
      geometry <- node.value.shape.flatMap(_.geometry)
      shape <- node.value.shape
    } yield {
      val nodeShape = new Rectangle {
        x = 0
        y = 0
        arcWidth = 5.0
        arcHeight = 5.0
        width = geometry.width
        height = geometry.height
        fill = Color.web(shape.fill.flatMap(_.color).getOrElse("#FFFFFF"))
        stroke = Color.web(shape.borderStyle.map(_.color).getOrElse("#CCCCCC"))
        strokeWidth = shape.borderStyle.map(_.width).getOrElse(0.0)
      }

      val textValue = shape.label.map(_.text).getOrElse("")

      val label = new Text() {
        text = textValue
        y = geometry.height / 2.0
        wrappingWidth = geometry.width
        textAlignment = TextAlignment.Center
        textOrigin = VPos.Center
      }

      val nodeGroup = new Group {
        layoutX = geometry.x
        layoutY = geometry.y
        children.add(nodeShape)
        children.add(label)
        onMouseClicked = _ => handleSelect(ElementRef(node.id, NodeType)).unsafeRunSync()
      }

      group.children.add(nodeGroup)

      JFXElement(node.id, nodeGroup, nodeGroup, label)
    }
  }

  override def setSelection(element: GraphElement[Node]): IO[Unit] = IO {
    element.selectElem.setEffect(new InnerShadow(15.0, Color.Green))
  }

  override def unsetSelection(element: GraphElement[Node]): IO[Unit] = IO {
    Option(element.selectElem.getEffect).foreach(_.asInstanceOf[javafx.scene.effect.InnerShadow].setRadius(0.0))
  }

  override def deleteElement(element: GraphElement[Node]): IO[Unit] = IO {
    group.children.remove(element.group)
  }

  override def reset: IO[Unit] = IO {
    transformation.setToIdentity()
  }
}
