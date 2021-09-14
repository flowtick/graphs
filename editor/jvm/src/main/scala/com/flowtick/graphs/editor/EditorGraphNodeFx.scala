package com.flowtick.graphs.editor

import cats.effect.IO
import com.flowtick.graphs.style._
import com.flowtick.graphs.layout.Geometry
import javafx.event.EventHandler
import javafx.scene.input.MouseEvent
import scalafx.geometry.VPos
import scalafx.scene.image.ImageView
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Ellipse, Rectangle, Shape}
import scalafx.scene.text.{Text, TextAlignment}
import scalafx.scene.transform.Affine
import scalafx.scene.{Group, Node}

class EditorGraphNodeFx(
    nodeId: String,
    geometry: Geometry,
    labelValue: Option[String],
    shape: NodeShape
)(
    transformation: Affine,
    handleSelect: ElementRef => Boolean => IO[Unit],
    handleDrag: Option[DragStart[Node]] => IO[Unit],
    handleDoubleClick: Any => IO[Unit]
) extends Group { self =>
  val defaultBorderStyle = shape.borderStyle.orElse(
    Some(BorderStyle("#888888", styleType = Some("line"), width = Some(1.0)))
  )

  lazy val fallBackShape = new Rectangle {
    x = 0
    y = 0
    width = geometry.width
    height = geometry.height
    fill = shape.fill.flatMap(_.color).map(Color.web).getOrElse(Color.Transparent)
    stroke = defaultBorderStyle.map(border => Color.web(border.color)) getOrElse Color.Transparent
    strokeWidth = defaultBorderStyle.flatMap(_.width).getOrElse(0.0)
  }

  val vectorShape: Shape = shape.shapeType match {
    case Some(ShapeType.Ellipse) =>
      new Ellipse {
        centerX = geometry.width / 2
        centerY = geometry.height / 2
        radiusX = geometry.width / 2
        radiusY = geometry.height / 2
        fill = shape.fill
          .flatMap(_.color)
          .map(Color.web)
          .getOrElse(Color.Transparent)
        stroke =
          shape.borderStyle.map(border => Color.web(border.color)) getOrElse Color.Transparent
        strokeWidth = shape.borderStyle.flatMap(_.width).getOrElse(0.0)
      }

    case Some(ShapeType.RoundRectangle) =>
      new Rectangle {
        x = 0
        y = 0
        arcWidth = 5.0
        arcHeight = 5.0
        width = geometry.width
        height = geometry.height
        fill = shape.fill
          .flatMap(_.color)
          .map(Color.web)
          .getOrElse(Color.Transparent)
        stroke =
          shape.borderStyle.map(border => Color.web(border.color)) getOrElse Color.Transparent
        strokeWidth = shape.borderStyle.flatMap(_.width).getOrElse(0.0)
      }
    case _ => fallBackShape
  }

  val nodeShape: Node = shape.image
    .flatMap(img => ImageLoaderFx.getImage(img))
    .orElse(
      shape.svgContent.flatMap(svg => ImageLoaderFx.getImage(svg.refId))
    ) match {
    case Some(loaded) =>
      new ImageView {
        fitWidth = geometry.width
        fitHeight = geometry.height
        image = loaded
      }

    case _ => vectorShape
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

  val label = new Text() {
    text = labelValue.getOrElse("")
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

  onMousePressed = new EventHandler[MouseEvent] {
    override def handle(event: MouseEvent): Unit = {
      handleSelect(ElementRef(nodeId, NodeType))(event.isControlDown)
        .unsafeRunSync()
    }
  }

  selectRect.onMousePressed = new EventHandler[MouseEvent] {
    override def handle(event: MouseEvent): Unit = {
      event.consume()
      val mousePos =
        transformation.inverseTransform(event.getSceneX, event.getSceneY)

      if (event.getClickCount == 2) {
        handleDoubleClick(event).unsafeRunSync()
      }

      if (event.isPrimaryButtonDown) {
        nodeDragStart = Some(
          DragStart(
            mousePos.getX,
            mousePos.getY,
            selectRect.x.value,
            selectRect.y.value,
            self,
            ElementRef(nodeId, NodeType),
            None,
            0.0,
            0.0
          )
        )
      }
    }
  }

  val dragHandler = new EventHandler[MouseEvent] {
    override def handle(event: MouseEvent): Unit = {
      event.consume()

      val mousePos =
        transformation.inverseTransform(event.getSceneX, event.getSceneY)

      if (event.isPrimaryButtonDown) {
        nodeDragStart = nodeDragStart.map { dragStart =>
          val deltaX = mousePos.getX - dragStart.cursorX
          val deltaY = mousePos.getY - dragStart.cursorY

          val newX = dragStart.transformX + deltaX
          val newY = dragStart.transformY + deltaY

          selectRect.x = newX
          selectRect.y = newY

          dragStart.copy(
            lastPos = Some(PagePoint(newX, newY)),
            deltaX = deltaX,
            deltaY = deltaY
          )
        }
      }
    }
  }

  selectRect.onMouseDragged = dragHandler

  val releaseHandler = new EventHandler[MouseEvent] {
    override def handle(event: MouseEvent): Unit = {
      nodeDragStart match {
        case Some(drag) if Math.abs(drag.deltaX) < 2 && Math.abs(drag.deltaY) < 2 =>
          handleSelect(drag.element)(false).unsafeRunSync()
        case _ =>
      }

      handleDrag(nodeDragStart).unsafeRunSync()
    }
  }

  selectRect.onMouseReleased = releaseHandler
}
