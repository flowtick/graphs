package com.flowtick.graphs.algorithm

import com.flowtick.graphs.Graph
import com.flowtick.graphs.algorithm.Traversal.Step

class TopologicalSort[E, N](graph: Graph[E, N]) {
  def sort: List[Step[E, N]] =
    new DepthFirstTraversal[E, N](graph.nodeIds, graph).completed
      .foldLeft(List.empty[Step[E, N]]) { case (acc, next) =>
        next :: acc // reverse the completed
      }
}
