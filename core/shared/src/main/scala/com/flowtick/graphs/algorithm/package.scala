package com.flowtick.graphs

package object algorithm {
  implicit class GraphOps[G[_, _, _], V, N, M](graph: G[V, N, M])(implicit graphType: Graph[G], identifiable: Identifiable[N]) {
    def bfs(startNode: N) = new BreadthFirstSearch[G, V, N, M](Seq(startNode), graph)
    def dfs(startNode: N) = new DepthFirstSearch[G, V, N, M](Seq(startNode), graph)
    def topologicalSort: List[N] = new TopologicalSort[G, V, N, M](graph).sort
    def shortestPath(implicit numeric: Numeric[V]) = new DijkstraShortestPath[G, V, N, M](graph)
  }

  implicit class Dijkstra[G[_, _, _], V, N, M](graph: G[V, N, M])(implicit
    numeric: Numeric[V],
    identifiable: Identifiable[N],
    graphType: Graph[G]) extends DijkstraShortestPath[G, V, N, M](graph)
}
