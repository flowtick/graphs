package com.flowtick.graphs.view.util

import com.flowtick.graphs.layout.{EdgePath, GraphLayoutLike, PointSpec}
import com.flowtick.graphs.{Edge, Graph}

object DrawUtil {
  def getLinePoints[E, N](
      edge: Edge[E],
      graph: Graph[E, N],
      layout: GraphLayoutLike
  ): Option[Iterator[PointSpec]] = for {
    fromNode <- graph.findNode(edge.from)
    toNode <- graph.findNode(edge.to)
    from <- layout.nodeGeometry(fromNode.id)
    to <- layout.nodeGeometry(toNode.id)

    fromCenterX = from.x + from.width / 2
    fromCenterY = from.y + from.height / 2

    toCenterX = to.x + to.width / 2
    toCenterY = to.y + to.height / 2

    path = layout.edgePath(edge.id).getOrElse(EdgePath())
    start = PointSpec(fromCenterX + path.sourceX, fromCenterY + path.sourceY)
    end = PointSpec(toCenterX + path.targetX, toCenterY + path.targetY)
    points = Iterator(start) ++ path.points ++ Iterator(end)
  } yield points
}
