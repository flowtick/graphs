package com.flowtick.graphs.algorithm

import com.flowtick.graphs.{Graph, Node}

import scala.collection.mutable

class TopologicalSort[E, N](graph: Graph[E, N]) extends DepthFirstSearch[E, N](graph.nodeIds, graph) {
  def sort: List[Node[N]] = {
    val sortedNodes = mutable.ListBuffer.empty[Node[N]]
    onComplete(sortedNodes.prepend(_)).run
    sortedNodes.toList
  }
}
