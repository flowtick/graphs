package com.flowtick.graphs.editor

import cats.effect.IO
import com.flowtick.graphs.graphml.GraphMLGraph
import com.flowtick.graphs.{DrawUtil, EditorComponent, EditorModel, SetGraph}
import io.circe.Json
import scalafx.geometry.VPos
import scalafx.scene.Group
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Polygon, Polyline, Rectangle}
import scalafx.scene.text.{Text, TextAlignment}

class EditorViewJavaFx(pane: EditorGraphPane) extends EditorComponent {
  override def init(model: EditorModel): IO[Unit] = IO.unit

  override def eval: Eval = ctx => ctx.effect(this) {
    case SetGraph(graphml) => addNodes(graphml) *> addEdges(graphml)
    case event => IO(println(event.getClass))
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

  def addNodes(graphml: GraphMLGraph[Json, Json]): IO[Unit] = IO {
    graphml.graph.nodes.foreach { node =>
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

        val label = shape.label.map(_.text).map(textValue => new Text() {
          text = textValue
          y = geometry.height / 2.0
          wrappingWidth = geometry.width
          textAlignment = TextAlignment.Center
          textOrigin = VPos.Center
        })

        pane.group.children.add(new Group {
          layoutX = geometry.x
          layoutY = geometry.y
          children.add(nodeShape)
          label.foreach(text => children.add(text))
        })
      }
    }
  }

  def addEdges(graphml: GraphMLGraph[Json, Json]): IO[Unit] = IO {
    graphml.graph.edges.foreach { edge =>
      for {
        edgePoints <- DrawUtil.getLinePoints(edge, graphml).map(_.toList.reverse)
        polyline = new Polyline {
          edgePoints.foreach(point => points.addAll(point.x, point.y))
          strokeWidth = 1.0
          stroke = Color.web("#000000")
        }
        last <- edgePoints.headOption
        secondLast <- edgePoints.tail.headOption

        arrowHead = createArrowHead(secondLast.x, secondLast.y, last.x, last.y)

      } yield {
        pane.group.children.add(polyline)
        pane.group.children.add(arrowHead)
      }
    }
  }

}