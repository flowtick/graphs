package com.flowtick.graphs

import cats.effect.IO
import com.flowtick.graphs.graphml.{EdgePath, EdgeShape, GraphMLEdge, GraphMLGraph, GraphMLNode, ShapeType}
import io.circe.Json

class RoutingFeature extends EditorComponent {

  def isRectangleNode(node: Node[GraphMLNode[Json]]): Boolean = node.value.shape.flatMap(_.shapeType) match {
    case Some(ShapeType.Rectangle) => true
    case Some(ShapeType.RoundRectangle) => true
    case _ => false
  }

  def edgePath(fromNode: Node[GraphMLNode[Json]], toNode: Node[GraphMLNode[Json]]): Option[EdgePath] =
    for {
      from <- fromNode.value.shape.flatMap(_.geometry)
      to <- toNode.value.shape.flatMap(_.geometry)
    } yield {
      val fromCenterX = from.x + from.width / 2
      val fromCenterY = from.y + from.height / 2

      val toCenterX = to.x + to.width / 2
      val toCenterY = to.y + to.height / 2

      val sourceCenter = Vector2(fromCenterX, fromCenterY)
      val targetCenter = Vector2(toCenterX, toCenterY)
      val edgeSegment = LineSegment(sourceCenter, targetCenter)

      val sourceRect = Rectangle(Vector2(from.x, from.y), Vector2(from.x + from.width, from.y + from.height))
      val targetRect = Rectangle(Vector2(to.x, to.y), Vector2(to.x + to.width, to.y + to.height))

      val sourcePort = if (isRectangleNode(fromNode))
        MathUtil.rectIntersect(edgeSegment, sourceRect).map(_ - sourceCenter)
      else None

      val targetPort = MathUtil.rectIntersect(edgeSegment, targetRect).map(_ - targetCenter)

      EdgePath(
        sourcePort.map(_.x).getOrElse(0.0),
        sourcePort.map(_.y).getOrElse(0.0),

        targetPort.map(_.x).getOrElse(0.0),
        targetPort.map(_.y).getOrElse(0.0),
        List.empty
      )
    }

  private def updateRouting(ctx: EditorContext, edge: Edge[GraphMLEdge[Json]]): EditorContext = {
    val fromNode = ctx.model.graphml.graph.findNode(edge.from)
    val toNode = ctx.model.graphml.graph.findNode(edge.to)

    val newPath = for {
      from <- fromNode
      to <- toNode
      path <- edgePath(from, to)
    } yield path

    val newShape: Option[EdgeShape] = edge.value.shape
      .map(_.copy(path = newPath))
      .orElse(Some(EdgeShape(path = newPath)))

    ctx
      .copy(model = ctx.model.copy(graphml = ctx.model.graphml.updateEdge(edge.id, _.copy(shape = newShape))))
      .addNotification(this, ElementUpdated(ElementRef(edge.id, EdgeType)))
  }

  override def eval: Eval = ctx => IO(ctx.transform {
    case ElementUpdated(ElementRef(id, EdgeType), Created) =>
      val newEdge = ctx.model.graphml.graph.findEdge(id)
      newEdge.map(updateRouting(ctx, _)).getOrElse(ctx)

    case ElementUpdated(ElementRef(id, NodeType), _) =>
      val edges = ctx.model.graphml.graph.incoming(id) ++ ctx.model.graphml.graph.outgoing(id)

      edges.foldLeft(ctx) {
        case (current, edge) => updateRouting(current, edge)
      }
  })
}
