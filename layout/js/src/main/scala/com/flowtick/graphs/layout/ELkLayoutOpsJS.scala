package com.flowtick.graphs.layout
import com.flowtick.graphs.{Edge, Graph, Labeled}

import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.{JSON, Thenable, |}

class ElkGraphLayoutLike(layoutGraph: ElkLayoutGraph) extends GraphLayoutLike {
  override def nodeGeometry(id: String): Option[Geometry] = ???

  override def setNodeGeometry(id: String, geometry: Geometry): GraphLayoutLike = ???

  override def updateNodePosition(id: String, fx: Double => Double, fy: Double => Double): GraphLayoutLike = ???

  override def edgePath(id: String): Option[EdgePath] = ???

  override def setEdgePath(id: String, edgePath: EdgePath): GraphLayoutLike = ???

  override def width: Option[Double] = layoutGraph.width.toOption

  override def height: Option[Double] = layoutGraph.height.toOption

  override def toGraphLayouts: List[GraphLayout] = ???
}

class ELkLayoutOpsJS(elkjs: ELKJS) extends GraphLayoutOps {
  private val handleError: js.Function1[Any, GraphLayout | Thenable[GraphLayout]] = (error: Any) => {
    println(error)
    GraphLayout()
  }

  private val elkResultToGraphLayout: js.Function1[ElkLayoutGraph, GraphLayout | Thenable[GraphLayout]] = (result: ElkLayoutGraph) => {
    org.scalajs.dom.window.asInstanceOf[js.Dynamic].result = result
    println("result", JSON.stringify(result))
    println(result.width)
    GraphLayout()
  }

  override def layout[E, N](g: Graph[E, N], layoutConfiguration: GraphLayoutConfiguration)(implicit edgeLabel: Labeled[Edge[E], String]): Future[GraphLayoutLike] = {
    val graph = new ElkLayoutGraph("graph",
      children = g.nodes.foldLeft(js.Array.apply[ElkLayoutNode]()) {
        case (acc, next) => acc :+ new ElkLayoutNode(next.id, layoutConfiguration.nodeWidth, layoutConfiguration.nodeHeight)
      },
      edges = g.edges.foldLeft(js.Array.apply[ElkLayoutEdge]()) {
      case (acc, next) => acc :+ new ElkLayoutEdge(next.id, js.Array(next.from), js.Array(next.to))
    })
    elkjs.layout(graph).`then`[GraphLayout](onFulfilled = elkResultToGraphLayout, onRejected = handleError).toFuture
  }
}
