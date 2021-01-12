package com.flowtick.graphs.algorithm

import com.flowtick.graphs.Graph
import com.flowtick.graphs.algorithm.Traversal.Step

class TopologicalSort[E, N](graph: Graph[E, N]) {
  def sort: List[Step[E, N]] = {
    val dfs = new DepthFirstTraversal[E, N](graph.nodeIds, graph).run

    dfs.foldLeft(List.empty[Step[E, N]]) {
      case (acc, Completed(step, _)) => step :: acc
      case (acc, _) => acc
    }
  }
}
