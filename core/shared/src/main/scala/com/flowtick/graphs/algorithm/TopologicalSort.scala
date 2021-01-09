package com.flowtick.graphs.algorithm

import com.flowtick.graphs.{Graph, Node}

import scala.collection.mutable

class TopologicalSort[E, N](graph: Graph[E, N]) {
  def sort: List[Node[N]] = {
    val dfs = new DepthFirstTraversal[E, N](graph.nodeIds, graph).run

    dfs.foreach(println)

    dfs.foldLeft(List.empty[Node[N]]) {
      case (acc, Completed(node)) => node :: acc
      case (acc, _) => acc
    }
  }
}
