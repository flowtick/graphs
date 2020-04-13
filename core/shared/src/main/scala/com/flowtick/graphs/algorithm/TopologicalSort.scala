package com.flowtick.graphs.algorithm

import com.flowtick.graphs.Graph

import scala.collection.mutable

class TopologicalSort[E, N](graph: Graph[E, N]) extends DepthFirstSearch[E, N](graph.contexts.keys, graph) {
  def sort: List[N] = {
    val sortedNodes = mutable.ListBuffer.empty[N]
    onComplete(sortedNodes.prepend).run
    sortedNodes.toList
  }
}
