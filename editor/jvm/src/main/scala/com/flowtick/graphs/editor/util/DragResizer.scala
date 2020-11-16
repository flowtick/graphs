package com.flowtick.graphs.editor.util

import javafx.event.EventHandler
import javafx.scene.Cursor
import javafx.scene.input.MouseEvent
import javafx.scene.layout.Region

/**
 *
 * adapted from https://gist.githubusercontent.com/cannibalsticky/a3057d6e9c1f029d99e6bc95f9b3340e/raw/b216499cf78322746709310079974d89ce52b885/DragResizerXY.java
 *
 */
class DragResizerXY(region: Region) {

  /**
   * The margin around the control that a user can click in to start resizing
   * the region.
   */
  val RESIZE_MARGIN = 10

  var x: Double = 0.0
  var y: Double = 0.0

  var initMinHeight: Boolean = false
  var initMinWidth: Boolean = false

  var draggableZoneX, draggableZoneY: Boolean = false
  var dragging: Boolean = false

  def mouseReleased(event: MouseEvent): Unit = {
    dragging = false
    region.setCursor(Cursor.DEFAULT)
  }

  def mouseOver(event: MouseEvent): Unit = {
    if (isInDraggableZone(event) || dragging) {
      if (draggableZoneY) {
        region.setCursor(Cursor.S_RESIZE)
      }

      if (draggableZoneX) {
        region.setCursor(Cursor.E_RESIZE)
      }

    } else {
      region.setCursor(Cursor.DEFAULT)
    }
  }


  //had to use 2 variables for the controll, tried without, had unexpected behaviour (going big was ok, going small nope.)
  def isInDraggableZone(event: MouseEvent): Boolean = {
    draggableZoneY = event.getY() > (region.getHeight() - RESIZE_MARGIN)
    draggableZoneX = event.getX() > (region.getWidth() - RESIZE_MARGIN)
    draggableZoneY || draggableZoneX
  }

  def mouseDragged(event: MouseEvent) {
    if (!dragging) {
      return
    }

    if (draggableZoneY) {
      val mousey = event.getY()

      val newHeight = region.getMinHeight() + (mousey - y)

      region.setMinHeight(newHeight)

      y = mousey
    }

    if (draggableZoneX) {
      val mousex = event.getX()

      val newWidth = region.getMinWidth() + (mousex - x)

      region.setMinWidth(newWidth)

      x = mousex
    }

  }

  def mousePressed(event: MouseEvent) {

    // ignore clicks outside of the draggable margin
    if (!isInDraggableZone(event)) {
      return
    }

    dragging = true

    // make sure that the minimum height is set to the current height once,
    // setting a min height that is smaller than the current height will
    // have no effect
    if (!initMinHeight) {
      region.setMinHeight(region.getHeight())
      initMinHeight = true
    }

    y = event.getY()

    if (!initMinWidth) {
      region.setMinWidth(region.getWidth())
      initMinWidth = true
    }

    x = event.getX()
  }
}

object DragResizer {
  def makeResizable(region: Region) = {
    val resizer = new DragResizerXY(region)

    region.setOnMousePressed(new EventHandler[MouseEvent]() {
      def handle(event: MouseEvent) {
        resizer.mousePressed(event)
      }
    })
    region.setOnMouseDragged(new EventHandler[MouseEvent] () {
      def handle(event: MouseEvent) {
        resizer.mouseDragged(event)
      }
    })
    region.setOnMouseMoved(new EventHandler[MouseEvent] () {
      def handle(event: MouseEvent) {
        resizer.mouseOver(event)
      }
    })
    region.setOnMouseReleased(new EventHandler[MouseEvent] () {
      def handle(event: MouseEvent) {
        resizer.mouseReleased(event)
      }
    })
  }
}
