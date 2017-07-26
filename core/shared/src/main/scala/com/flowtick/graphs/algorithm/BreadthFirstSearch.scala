package com.flowtick.graphs.algorithm

import com.flowtick.graphs.traversal.{ AdjacentNodesTraversal, FifoBuffer, Traversal }
import com.flowtick.graphs.{ Edge, Graph, Node }

class BreadthFirstSearch[N <: Node, E <: Edge[N]](graph: Graph[N, E]) {
  def traverse(startNode: Option[N] = None): Traversal[N] =
    new AdjacentNodesTraversal[N, E](startNode, graph, () => new FifoBuffer[N]())
}
