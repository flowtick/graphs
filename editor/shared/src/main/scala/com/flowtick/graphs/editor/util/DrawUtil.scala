package com.flowtick.graphs.editor.util

import com.flowtick.graphs.editor.{EditorGraphEdge, EditorGraphLayoutLike, EditorGraphNode}
import com.flowtick.graphs.style.PointSpec
import com.flowtick.graphs.{Edge, Graph}

object DrawUtil {
  def getLinePoints(edge: Edge[EditorGraphEdge],
                    graph: Graph[EditorGraphEdge, EditorGraphNode],
                    layout: EditorGraphLayoutLike): Option[Iterator[PointSpec]] = for {
    fromNode <- graph.findNode(edge.from)
    toNode <- graph.findNode(edge.to)
    from <- layout.nodeGeometry(fromNode.id)
    to <- layout.nodeGeometry(toNode.id)

    fromCenterX = from.x + from.width / 2
    fromCenterY = from.y + from.height / 2

    toCenterX = to.x + to.width / 2
    toCenterY = to.y + to.height / 2

    path <- layout.edgePath(edge.id)
    start = PointSpec(fromCenterX + path.sourceX, fromCenterY + path.sourceY)
    end = PointSpec(toCenterX + path.targetX, toCenterY + path.targetY)
    points = Iterator(start) ++ path.points ++ Iterator(end)
  } yield points
}
