package com.flowtick.graphs.algorithm

import com.flowtick.graphs.{ EdgeType, Graph }

import scala.collection.mutable

class TopologicalSort[G[_, _, _], E[_, _], V, N, M](graph: G[E[V, N], N, M])(implicit
  graphType: Graph[G, E],
  edgeType: EdgeType[E]) extends DepthFirstSearch[G, E, V, N, M](graphType.nodes(graph), graph) {
  def sort: List[N] = {
    val sortedNodes = mutable.ListBuffer.empty[N]
    onComplete(sortedNodes.prepend(_)).run
    sortedNodes.toList
  }
}
