package com.flowtick.graphs.algorithm

import com.flowtick.graphs.Graph

import scala.collection.mutable

class TopologicalSort[V, N, M](graph: Graph[V, N, M]) extends DepthFirstSearch[V, N, M](graph.nodes, graph) {
  def sort: List[N] = {
    val sortedNodes = mutable.ListBuffer.empty[N]
    onComplete(sortedNodes.prepend(_)).run
    sortedNodes.toList
  }
}
