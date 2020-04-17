package com.flowtick.graphs

package object algorithm {
  implicit class GraphOps[M, E, N](graph: Graph[M, E, N]) {
    def bfs(startNode: N) = new BreadthFirstSearch[M, E, N](Seq(startNode), graph)
    def dfs(startNode: N) = new DepthFirstSearch[M, E, N](Seq(startNode), graph)
    def topologicalSort: List[N] = new TopologicalSort[M, E, N](graph).sort
    def dijkstra(implicit numeric: Numeric[E],
                 label: Labeled[Edge[E, N], E]) = new DijkstraShortestPath[M, E, N](graph)
  }
}
