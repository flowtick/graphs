package com.flowtick.graphs

package object algorithm {
  implicit class GraphOps[M, E, N](graph: Graph[E, N]) {
    def bfs(startNode: String) = new BreadthFirstTraversal[E, N](Seq(startNode), graph)
    def dfs(startNode: String) = new DepthFirstTraversal[E, N](Seq(startNode), graph)
    def topologicalSort: List[Node[N]] = new TopologicalSort[E, N](graph).sort
    def dijkstra(implicit numeric: Numeric[E],
                 label: Labeled[Edge[E], E]) = new DijkstraShortestPath[M, E, N](graph)
  }
}
