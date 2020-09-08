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

  def updateEdgeRouting(edge: Edge[GraphMLEdge[Json]], graphml: GraphMLGraph[Json, Json]): Option[GraphMLGraph[Json, Json]] =
    for {
      fromNode <- graphml.graph.findNode(edge.from)
      _ = println(fromNode)
      toNode <- graphml.graph.findNode(edge.to)
      _ = println(toNode)
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

      val sourceRect = Rectangle(Vector2(from.x, from.y), Vector2(from.x + from.width, from.y +  from.height))
      val targetRect = Rectangle(Vector2(to.x, to.y), Vector2(to.x + to.width, to.y +  to.height))

      val sourcePort = if (isRectangleNode(fromNode))
        MathUtil.rectIntersect(edgeSegment, sourceRect).map(_ - sourceCenter)
      else None

      val targetPort = MathUtil.rectIntersect(edgeSegment, targetRect).map(_ - targetCenter)

      val newPath = EdgePath(
        sourcePort.map(_.x).getOrElse(0.0),
        sourcePort.map(_.y).getOrElse(0.0),

        targetPort.map(_.x).getOrElse(0.0),
        targetPort.map(_.y).getOrElse(0.0),
        List.empty
      )

      val shape: Option[EdgeShape] = edge.value.shape
        .map(_.copy(path = Some(newPath)))
        .orElse(Some(EdgeShape(path = Some(newPath))))

    graphml.updateEdge(edge.id, edge => edge.copy(shape = shape))
  }

  override def eval: Eval = ctx => IO(ctx.transform {
    case AddEdge(id, _, _, _) =>
      val edge = ctx.model.graphml.graph.findEdge(id)

      edge
        .flatMap(updateEdgeRouting(_, ctx.model.graphml))
        .fold(ctx)(graphml => ctx.copy(model = ctx.model.updateGraphMl(graphml)))
        .addNotification(this, ElementUpdated(ElementRef(id, EdgeType)))

    case Move(ElementRef(id, NodeType), _, _) =>
      val edges = ctx.model.graphml.graph.incoming(id) ++ ctx.model.graphml.graph.outgoing(id)

      println(s"edges for related to move: $edges")

      edges.foldLeft(ctx) {
        case (ctx, edge) => 
        ctx
          .copy(model = ctx.model.updateGraphMl(updateEdgeRouting(edge, ctx.model.graphml).getOrElse(ctx.model.graphml)))
          .addNotification(this, ElementUpdated(ElementRef(edge.id, EdgeType)))
      }
  })
}
