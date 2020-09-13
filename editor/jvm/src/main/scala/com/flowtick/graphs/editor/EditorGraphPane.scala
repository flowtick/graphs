package com.flowtick.graphs.editor

import javafx.scene.input.{MouseEvent, ScrollEvent}
import scalafx.scene.Group
import scalafx.scene.layout.{BorderPane, Pane}
import scalafx.scene.paint.Color
import scalafx.scene.shape.Line

class EditorGraphPane extends BorderPane {
  val group = new Pane {
    viewOrder_(1.0)
  }

  center = group

  addOriginCross

  import javafx.event.EventHandler
  val SCALE_DELTA = 1.25

  def clear() = {
    group.children.clear()
    addOriginCross
  }

  this.setOnScroll(new EventHandler[ScrollEvent]() {
    override def handle(event: ScrollEvent): Unit = {
      event.consume()

      if (event.getDeltaY != 0) {
        val scaleFactor = if (event.getDeltaY > 0) SCALE_DELTA else 1 / SCALE_DELTA

        group.scaleX = group.scaleX.value * scaleFactor
        group.scaleY = group.scaleY.value * scaleFactor
      }
    }
  })

  case class DragContext(mouseAnchorX: Double, mouseAnchorY: Double, translateAnchorX: Double, translateAnchorY: Double)

  var dragContext = DragContext(0.0, 0.0, 0.0, 0.0)

  this.setOnMousePressed(new EventHandler[MouseEvent] {
    override def handle(event: MouseEvent): Unit = {
      event.consume()

      if (!event.isPrimaryButtonDown) {
        dragContext = dragContext.copy(mouseAnchorX = event.getX)
        dragContext = dragContext.copy(mouseAnchorY = event.getY)

        dragContext = dragContext.copy(translateAnchorX = group.translateX.value)
        dragContext = dragContext.copy(translateAnchorY = group.translateY.value)
      }
    }
  })

  this.setOnMouseDragged(new EventHandler[MouseEvent] {
    override def handle(event: MouseEvent): Unit = {
      event.consume()

      if (!event.isPrimaryButtonDown) {
        group.translateX = (dragContext.translateAnchorX + ((event.getSceneX - dragContext.mouseAnchorX)))
        group.translateY = (dragContext.translateAnchorY + ((event.getSceneY - dragContext.mouseAnchorY)))
        group.layout()
        parent.value.layout()
      }
    }
  })

  // draw origin cross
  def addOriginCross = {
    group.children.add(new Group {
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
    })
  }

}
