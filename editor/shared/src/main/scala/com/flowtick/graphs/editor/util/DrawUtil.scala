package com.flowtick.graphs.editor.util

import com.flowtick.graphs.Edge
import com.flowtick.graphs.editor.{EditorGraph, EditorGraphEdge}
import com.flowtick.graphs.style.PointSpec

object DrawUtil {
  def getLinePoints(edge: Edge[EditorGraphEdge],
                    editorGraph: EditorGraph): Option[Iterator[PointSpec]] = for {
    fromNode <- editorGraph.graph.findNode(edge.from)
    toNode <- editorGraph.graph.findNode(edge.to)
    from <- editorGraph.layout.nodes.get(fromNode.id)
    to <- editorGraph.layout.nodes.get(toNode.id)

    fromCenterX = from.x + from.width / 2
    fromCenterY = from.y + from.height / 2

    toCenterX = to.x + to.width / 2
    toCenterY = to.y + to.height / 2

    path <- editorGraph.layout.edges.get(edge.id)
    start = PointSpec(fromCenterX + path.sourceX, fromCenterY + path.sourceY)
    end = PointSpec(toCenterX + path.targetX, toCenterY + path.targetY)
    points = Iterator(start) ++ path.points ++ Iterator(end)
  } yield points
}
