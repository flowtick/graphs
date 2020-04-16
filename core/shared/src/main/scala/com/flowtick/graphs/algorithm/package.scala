package com.flowtick.graphs

package object algorithm {
  implicit class GraphOps[E, N, M](graph: Graph[E, N, M]) {
    def bfs(startNode: N) = new BreadthFirstSearch[E, N, M](Seq(startNode), graph)
    def dfs(startNode: N) = new DepthFirstSearch[E, N, M](Seq(startNode), graph)
    def topologicalSort: List[N] = new TopologicalSort[E, N, M](graph).sort
    def dijkstra(implicit numeric: Numeric[E],
                 label: Labeled[Edge[E, N], E]) = new DijkstraShortestPath[E, N, M](graph)
  }
}
