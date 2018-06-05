package com.flowtick.graphs.algorithm

import com.flowtick.graphs.Graph

import scala.collection.mutable

class TopologicalSort[N, E](graph: Graph[N, E]) extends DepthFirstSearch[N, E](graph.nodes, graph) {
  def sort: List[N] = {
    val sortedNodes = mutable.ListBuffer.empty[N]
    onComplete(sortedNodes.prepend(_)).run
    sortedNodes.toList
  }
}
