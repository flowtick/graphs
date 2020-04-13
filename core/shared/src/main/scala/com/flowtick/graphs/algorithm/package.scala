package com.flowtick.graphs

package object algorithm {
  implicit class GraphOps[E, N](graph: Graph[E, N]) {
    def bfs(startNode: N) = new BreadthFirstSearch[E, N](Seq(startNode), graph)
    def dfs(startNode: N) = new DepthFirstSearch[E, N](Seq(startNode), graph)
    def topologicalSort: List[N] = new TopologicalSort[E, N](graph).sort
    def dijkstra(implicit numeric: Numeric[E],
                 label: Labeled[Edge[E, N], E]) = new DijkstraShortestPath[E, N](graph)
  }
}
