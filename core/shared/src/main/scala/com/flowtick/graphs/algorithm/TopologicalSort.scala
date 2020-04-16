package com.flowtick.graphs.algorithm

import com.flowtick.graphs.Graph

import scala.collection.mutable

class TopologicalSort[E, N, M](graph: Graph[E, N, M]) extends DepthFirstSearch[E, N, M](graph.contexts.keys, graph) {
  def sort: List[N] = {
    val sortedNodes = mutable.ListBuffer.empty[N]
    onComplete(sortedNodes.prepend(_)).run
    sortedNodes.toList
  }
}
