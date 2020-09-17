package com.flowtick.graphs.editor

import cats.effect.IO
import com.flowtick.graphs.{DragStart, ElementRef, NodeType}
import com.flowtick.graphs.graphml.{BorderStyle, NodeShape}
import com.flowtick.graphs.layout.DefaultGeometry
import javafx.event.EventHandler
import javafx.scene.input.MouseEvent
import scalafx.geometry.VPos
import scalafx.scene.{Group, Node}
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle
import scalafx.scene.text.{Text, TextAlignment}
import scalafx.scene.transform.Affine

class EditorGraphNode(nodeId: String,
                      geometry: DefaultGeometry,
                      shape: NodeShape)(
                      transformation: Affine,
                      handleSelect: ElementRef => IO[Unit],
                      handleDrag: Option[DragStart[Node]] => IO[Unit]) extends Group { self =>
  val borderStyle = shape.borderStyle.orElse(Some(BorderStyle("#888888", styleType = "line", width = 1.0)))

  val nodeShape = new Rectangle {
    x = 0
    y = 0
    arcWidth = 5.0
    arcHeight = 5.0
    width = geometry.width
    height = geometry.height
    fill = Color.web(shape.fill.flatMap(_.color).getOrElse("#FFFFFF"))
    stroke = borderStyle.map(border => Color.web(border.color)) getOrElse Color.Transparent
    strokeWidth = borderStyle.map(_.width).getOrElse(0.0)
  }

  val selectRect = new Rectangle {
    x = geometry.x - 5
    y = geometry.y - 5
    width = geometry.width + 10
    height = geometry.height + 10
    stroke = Color.DarkGrey
    fill = new Color(Color.DarkGrey.opacity(0.1))
    strokeDashArray += 5
    visible = false
    viewOrder_(-20)
  }

  val textValue = shape.label.map(_.text).getOrElse("")

  val label = new Text() {
    text = textValue
    y = geometry.height / 2.0
    wrappingWidth = geometry.width
    textAlignment = TextAlignment.Center
    textOrigin = VPos.Center
  }

  layoutX = geometry.x
  layoutY = geometry.y

  children.add(nodeShape)
  children.add(label)

  var nodeDragStart: Option[DragStart[Node]] = None

  val pressedHandler = new EventHandler[MouseEvent] {
    override def handle(event: MouseEvent): Unit = {
      event.consume()

      val mousePos = transformation.inverseTransform(event.getSceneX, event.getSceneY)

      handleSelect(ElementRef(nodeId, NodeType)).unsafeRunSync()

      if (event.isPrimaryButtonDown) {
        nodeDragStart = Some(DragStart(mousePos.getX, mousePos.getY, selectRect.x.value, selectRect.y.value, self, ElementRef(nodeId, NodeType), None))
      }
    }
  }

  selectRect.onMousePressed = pressedHandler
  onMousePressed = pressedHandler

  val dragHandler = new EventHandler[MouseEvent] {
    override def handle(event: MouseEvent): Unit = {
      event.consume()

      val mousePos = transformation.inverseTransform(event.getSceneX, event.getSceneY)

      if (event.isPrimaryButtonDown) {
        nodeDragStart = nodeDragStart.map { dragStart =>
          val newX = dragStart.transformX + ((mousePos.getX - dragStart.cursorX))
          val newY = dragStart.transformY + ((mousePos.getY - dragStart.cursorY))

          selectRect.x = newX
          selectRect.y = newY

          dragStart.copy(lastPos = Some(newX, newY))
        }
      }
    }
  }

  selectRect.onMouseDragged = dragHandler
  onMouseDragged = dragHandler

  val releaseHandler = new EventHandler[MouseEvent] {
    override def handle(t: MouseEvent): Unit = {
      t.consume()
      handleDrag(nodeDragStart).unsafeRunSync()
      nodeDragStart = None
    }
  }

  selectRect.onMouseReleased = releaseHandler
  onMouseReleased = releaseHandler
}
