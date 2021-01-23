package com.flowtick.graphs.editor.feature

import cats.effect.IO
import com.flowtick.graphs.editor._
import com.flowtick.graphs.editor.util.MathUtil
import com.flowtick.graphs.{Edge, Node}
import com.flowtick.graphs.editor.util.MathUtil.{LineSegment, Rectangle, Vector2}
import com.flowtick.graphs.layout.{EdgePath, GraphLayoutLike}

class RoutingFeature extends EditorComponent {

  override def order: Double = 0.2

  def edgePath(fromNode: Node[EditorGraphNode], toNode: Node[EditorGraphNode])(layout: GraphLayoutLike): Option[EdgePath] =
    for {
      from <- layout.nodeGeometry(fromNode.id)
      to <- layout.nodeGeometry(toNode.id)
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

      val sourcePort = MathUtil.rectIntersect(edgeSegment, sourceRect).map(_ - sourceCenter)
      val targetPort = MathUtil.rectIntersect(edgeSegment, targetRect).map(_ - targetCenter)

      EdgePath(
        sourcePort.map(_.x).getOrElse(0.0),
        sourcePort.map(_.y).getOrElse(0.0),

        targetPort.map(_.x).getOrElse(0.0),
        targetPort.map(_.y).getOrElse(0.0),
        List.empty
      )
    }

  private def updateRouting(ctx: EditorContext, edge: Edge[EditorGraphEdge]): EditorContext = {
    val fromNode = ctx.model.graph.findNode(edge.from)
    val toNode = ctx.model.graph.findNode(edge.to)

    val newLayout = for {
      from <- fromNode
      to <- toNode
      path <- edgePath(from, to)(ctx.model.layout)
    } yield ctx.model.layout.setEdgePath(edge.id, path)

    ctx
      .updateModel(_.updateLayout(current => newLayout.getOrElse(current)))
      .addNotification(this, ElementUpdated(ElementRef(edge.id, EdgeType), Internal))
  }

  override def eval: Eval = ctx => IO(ctx.transform {
    case ElementUpdated(ElementRef(id, EdgeType), Created, _) =>
      val newEdge = ctx.model.graph.findEdge(id)
      newEdge.map(updateRouting(ctx, _)).getOrElse(ctx)

    case ElementUpdated(ElementRef(id, NodeType), _, _) =>
      val edges = ctx.model.graph.incoming(id) ++ ctx.model.graph.outgoing(id)

      edges.foldLeft(ctx) {
        case (current, edge) => updateRouting(current, edge)
      }
  })
}
