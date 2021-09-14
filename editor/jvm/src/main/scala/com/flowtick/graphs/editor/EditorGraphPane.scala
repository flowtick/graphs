package com.flowtick.graphs.editor

import cats.effect.IO
import com.flowtick.graphs
import com.flowtick.graphs._
import com.flowtick.graphs.editor.util.DrawUtil
import com.flowtick.graphs.editor.view.GraphElement
import com.flowtick.graphs.layout.{DefaultGeometry, PointSpec}
import javafx.event.EventHandler
import javafx.scene.input.{MouseEvent, ScrollEvent}
import scalafx.scene.layout.{BorderPane, Pane, Priority}
import scalafx.scene.paint.Color
import scalafx.scene.shape.Line._
import scalafx.scene.shape.{Circle, Line, Polygon, Polyline}
import scalafx.scene.text.{Text, TextAlignment}
import scalafx.scene.transform.Affine
import scalafx.scene.{Group, Node}

final case class JFXElement(id: ElementRef, group: Node, selectElem: Option[Node], label : Node) extends GraphElement[Node]

class EditorGraphPane(layout: BorderPane)(handleSelect: ElementRef => Boolean => IO[Unit],
                      handleDrag: Option[DragStart[Node]] => IO[Unit],
                      handleDoubleClick: Any => IO[Unit]) extends BorderPane with Page[Node, MouseEvent]{
  var panContext = PanContext(0.0, 0.0, 0.0, 0.0)

  val transformation = new Affine()

  val group = new Pane {
    viewOrder_(1.0)
    children.add(createOriginCross)
    transforms.add(transformation)
  }

  val scaleDelta = 1.25

  center = group

  vgrow = Priority.Always
  hgrow = Priority.Always

  layout.onScroll = new EventHandler[ScrollEvent]() {
    override def handle(event: ScrollEvent): Unit = {
      event.consume()

      if (event.getDeltaY != 0) {
        val scaleFactor = if (event.getDeltaY > 0) scaleDelta else 1 / scaleDelta

        val pivot = transformation.inverseTransform(event.getX - layoutX.value, event.getY - layoutY.value)
        transformation.appendScale(scaleFactor, scaleFactor, pivot.getX, pivot.getY)
      }
    }
  }

  layout.onMousePressed = new EventHandler[MouseEvent] {
    override def handle(event: MouseEvent): Unit = {
      event.consume()

      if (event.getClickCount == 2) {
        handleDoubleClick(()).unsafeRunSync()
      }

      if (!event.isPrimaryButtonDown) {
        panContext = panContext.copy(mouseAnchorX = event.getX)
        panContext = panContext.copy(mouseAnchorY = event.getY)

        panContext = panContext.copy(translateAnchorX = transformation.getTx)
        panContext = panContext.copy(translateAnchorY = transformation.getTy)
      }
    }
  }

  layout.onMouseDragged = new EventHandler[MouseEvent] {
    override def handle(event: MouseEvent): Unit = {
      event.consume()

      if (!event.isPrimaryButtonDown) {
        val newX = panContext.translateAnchorX + ((event.getX - panContext.mouseAnchorX))
        val newY = panContext.translateAnchorY + ((event.getY - panContext.mouseAnchorY))

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

  override def addEdge(edge: Edge[EditorGraphEdge], editorModel: EditorModel): IO[Option[GraphElement[Node]]] = IO {
    for {
      edgePoints <- DrawUtil.getLinePoints(edge, editorModel.graph, editorModel.layout).map(_.toList.reverse)
      last <- edgePoints.headOption
      secondLast <- edgePoints.tail.headOption

      arrowHead = createArrowHead(secondLast.x, secondLast.y, last.x, last.y)
    } yield {
      val textValue = edge.value.label.getOrElse("")

      val edgeLine = new Polyline {
        edgePoints.foreach(point => points.addAll(point.x, point.y))
        strokeWidth = 1.0
        stroke = Color.web("#000000")
      }

      val points = new Group {
        edgePoints.foreach(point => {
          val cirlce = new Circle {
            centerX = point.x
            centerY = point.y
            radius = 2
            fill = new Color(Color.Yellow)
            strokeWidth = 0.5
            stroke = Color.Black
          }
          children.add(cirlce)
        })

        visible = false
      }

      val selectLine = new Polyline {
        edgePoints.foreach(point => points.addAll(point.x, point.y))
        strokeWidth = 10.0
        stroke = Color.Transparent
        viewOrder_(2.0)
      }

      val selectGroup = new Group {
        override def visible_=(v: Boolean): Unit = {
          points.visible = v

          if (v) {
            selectLine.stroke = new Color(Color.DarkGrey.opacity(0.3))
          } else selectLine.stroke = Color.Transparent
        }
      }

      selectGroup.children.add(selectLine)
      selectGroup.children.add(points)

      val label = new Text() {
        text = textValue
        x = secondLast.x
        y = secondLast.y
        textAlignment = TextAlignment.Center
      }

      val edgeGroup = new Group {
        children.add(edgeLine)
        children.add(arrowHead)
        children.add(selectGroup)
        children.add(label)
        viewOrder_(-10)
      }

      selectLine.onMousePressed = new EventHandler[MouseEvent] {
        override def handle(t: MouseEvent): Unit = {
          handleSelect(ElementRef(edge.id, EdgeType))(t.isControlDown).unsafeRunSync()
          selectGroup.visible = true
        }
      }

      group.children.add(edgeGroup)

      JFXElement(ElementRef(edge.id, EdgeType), edgeGroup, Some(selectGroup), label)
    }
  }

  override def addNode(node: graphs.Node[EditorGraphNode], editorModel: EditorModel): IO[Option[GraphElement[Node]]] = IO {
    val geometry = editorModel.layout.nodeGeometry(node.id).getOrElse(DefaultGeometry(0.0, 0.0, 50, 50))
    val shape = editorModel.styleSheet.requireNodeStyle(Some(node.id), node.value.stencil.toList)
    val graphNode = new EditorGraphNodeFx(node.id, geometry, node.value.label, shape)(transformation, handleSelect, handleDrag, handleDoubleClick)
    group.children.add(graphNode)
    group.children.add(graphNode.selectRect)

    Some(JFXElement(ElementRef(node.id, NodeType), graphNode, Some(graphNode.selectRect), graphNode.label))
  }

  override def setSelection(element: GraphElement[Node]): IO[Unit] = element.id.elementType match {
    case NodeType => IO(element.selectElem.foreach(_.setVisible(true)))
    case EdgeType => IO(element.selectElem.foreach(_.visible = true))
  }

  override def unsetSelection(element: GraphElement[Node]): IO[Unit] = element.id.elementType match {
    case NodeType => IO(element.selectElem.foreach(_.setVisible(false)))
    case EdgeType => IO(element.selectElem.foreach(_.visible = false))
  }

  override def deleteElement(element: GraphElement[Node]): IO[Unit] = IO {
    group.children.remove(element.group)
    group.children.remove(element.selectElem)
  }

  override def resetTransformation: IO[Unit] = IO {
    transformation.setToIdentity()
  }

  override def screenCoordinates(x: Double, y: Double): Point = throw new UnsupportedOperationException("not implemented yet")

  override def pageCoordinates(x: Double, y: Double): Point = throw new UnsupportedOperationException("not implemented yet")

  override def beforeDrag: MouseEvent => Unit = throw new UnsupportedOperationException("not implemented yet")

  override def eventCoordinates(event: MouseEvent): Point = throw new UnsupportedOperationException("not implemented yet")

  override def applyDrag: DragStart[Node] => Unit = throw new UnsupportedOperationException("not implemented yet")
}
