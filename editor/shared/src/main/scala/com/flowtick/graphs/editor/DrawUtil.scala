package com.flowtick.graphs.editor

import com.flowtick.graphs.Edge
import com.flowtick.graphs.graphml.{EdgePath, EdgeShape, GraphMLEdge, GraphMLGraph, PointSpec}
import io.circe.Json

object DrawUtil {
  def getLinePoints(edge: Edge[GraphMLEdge[Json]],
                    graphml: GraphMLGraph[Json, Json]): Option[Iterator[PointSpec]] = for {
    fromNode <- graphml.graph.findNode(edge.from)
    toNode <- graphml.graph.findNode(edge.to)
    shape <- edge.value.shape.orElse(Some(EdgeShape()))
    from <- fromNode.value.shape.flatMap(_.geometry)
    to <- toNode.value.shape.flatMap(_.geometry)

    fromCenterX = from.x + from.width / 2
    fromCenterY = from.y + from.height / 2

    toCenterX = to.x + to.width / 2
    toCenterY = to.y + to.height / 2

    path <- shape.path.orElse(Some(EdgePath(0, 0, 0, 0, List.empty)))
    start = PointSpec(fromCenterX + path.sourceX, fromCenterY + path.sourceY)
    end = PointSpec(toCenterX + path.targetX, toCenterY + path.targetY)
    points = Iterator(start) ++ path.points ++ Iterator(end)
  } yield points
}
