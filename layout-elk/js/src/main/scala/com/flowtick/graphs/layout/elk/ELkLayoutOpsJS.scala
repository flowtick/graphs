package com.flowtick.graphs.layout.elk

import com.flowtick.graphs.layout._
import com.flowtick.graphs.{Edge, Graph, Labeled}

import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.{Promise, Thenable, |}
import scala.util.Try

class ELkLayoutOpsJS(elkjs: ELKJS) extends GraphLayoutOps {
  private val handleError: js.Function1[Any, GraphLayoutLike | Thenable[GraphLayoutLike]] =
    (error: Any) => Promise.reject(s"unable to layout graph with ELKJS: $error")

  private def elkEdgeToPath(
      elkEdge: ElkLayoutEdge
  ): Option[(String, EdgePath)] =
    elkEdge.sections.headOption
      .map(section =>
        elkEdge.id -> EdgePath(
          section.startPoint.x,
          section.startPoint.y,
          section.endPoint.x,
          section.endPoint.y,
          points = section.bendPoints
            .map(points => points.map(point => PointSpec(point.x, point.y)).toList)
            .getOrElse(List.empty)
        )
      )

  private val elkResultToGraphLayout: js.Function1[ElkLayoutGraph, GraphLayoutLike | Thenable[
    GraphLayoutLike
  ]] = (result: ElkLayoutGraph) => {
    Try(org.scalajs.dom.window.asInstanceOf[js.Dynamic].lastLayout = result)
    GraphLayout(
      width = result.width.toOption,
      height = result.height.toOption,
      nodes = result.children
        .map(elkNode =>
          elkNode.id -> DefaultGeometry(
            elkNode.x,
            elkNode.y,
            elkNode.width,
            elkNode.height
          )
        )
        .toMap,
      edges = result.edges
        .flatMap(elkEdgeToPath)
        .toMap
    )
  }

  override def layout[E, N](
      g: Graph[E, N],
      layoutConfiguration: GraphLayoutConfiguration
  )(implicit edgeLabel: Labeled[Edge[E], String]): Future[GraphLayoutLike] = {
    val graph = new ElkLayoutGraph(
      "graph",
      children = g.nodes.foldLeft(js.Array.apply[ElkLayoutNode]()) { case (acc, next) =>
        acc :+ new ElkLayoutNode(
          id = next.id,
          width = layoutConfiguration.nodeWidth,
          height = layoutConfiguration.nodeHeight
        )
      },
      edges = g.edges.foldLeft(js.Array.apply[ElkLayoutEdge]()) { case (acc, next) =>
        acc :+ new ElkLayoutEdge(
          next.id,
          js.Array(next.from),
          js.Array(next.to)
        )
      }
    )
    elkjs
      .layout(graph)
      .`then`[GraphLayoutLike](
        onFulfilled = elkResultToGraphLayout,
        onRejected = handleError
      )
      .toFuture
  }
}
