package com.flowtick.graphs

package object algorithm {
  implicit class GraphOps[G[_, _, _], V, N, M](graph: Graph[V, N, M])(implicit identifiable: Identifiable[N]) {
    def bfs(startNode: N) = new BreadthFirstSearch[V, N, M](Seq(startNode), graph)
    def dfs(startNode: N) = new DepthFirstSearch[V, N, M](Seq(startNode), graph)
    def topologicalSort: List[N] = new TopologicalSort[V, N, M](graph).sort
    def shortestPath(implicit numeric: Numeric[V]) = new DijkstraShortestPath[V, N, M](graph)
  }

  implicit class Dijkstra[V, N, M](graph: Graph[V, N, M])(implicit
    numeric: Numeric[V],
    identifiable: Identifiable[N]) extends DijkstraShortestPath[V, N, M](graph)
}
