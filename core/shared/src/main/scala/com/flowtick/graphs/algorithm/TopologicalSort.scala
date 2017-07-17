package com.flowtick.graphs.algorithm

import com.flowtick.graphs.{ Edge, Graph, Node }

import scala.collection.mutable

class TopologicalSort[N <: Node, E <: Edge[N]](graph: Graph[N, E]) extends DepthFirstSearch[N, E](graph) {
  def topologicalSort: List[N] = {
    val sortedNodes = mutable.ListBuffer.empty[N]
    dfs(None).onComplete(sortedNodes.prepend(_)).run
    sortedNodes.toList
  }
}
