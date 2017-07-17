package com.flowtick.graphs.algorithm

import com.flowtick.graphs.traversal.{ AdjacentNodesTraversal, LifoBuffer, Traversal }
import com.flowtick.graphs.{ Edge, Graph, Node }

class DepthFirstSearch[N <: Node, E <: Edge[N]](graph: Graph[N, E]) {
  def find(startNode: Option[N] = None): Traversal[N] =
    new AdjacentNodesTraversal[N, E](startNode, graph, () => new LifoBuffer[N]())
}
