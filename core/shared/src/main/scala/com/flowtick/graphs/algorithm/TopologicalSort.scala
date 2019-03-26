package com.flowtick.graphs.algorithm

import com.flowtick.graphs.Graph

import scala.collection.mutable

class TopologicalSort[G[_, _, _], V, N, M](graph: G[V, N, M])(implicit graphType: Graph[G]) extends DepthFirstSearch[G, V, N, M](graphType.nodes(graph), graph) {
  def sort: List[N] = {
    val sortedNodes = mutable.ListBuffer.empty[N]
    onComplete(sortedNodes.prepend(_)).run
    sortedNodes.toList
  }
}
