package com.flowtick.graphs

package object algorithm {
  implicit class GraphOps[G[_, _, _], E[_, _], V, N, M](graph: G[E[V, N], N, M])(implicit graphType: Graph[G], edgeType: EdgeType[E], identifiable: Identifiable[N]) {
    def bfs(startNode: N) = new BreadthFirstSearch[G, E, V, N, M](Seq(startNode), graph)
    def dfs(startNode: N) = new DepthFirstSearch[G, E, V, N, M](Seq(startNode), graph)
    def topologicalSort: List[N] = new TopologicalSort[G, E, V, N, M](graph).sort
    def shortestPath(implicit numeric: Numeric[V]) = new DijkstraShortestPath[G, E, V, N, M](graph)
  }

  implicit class Dijkstra[G[_, _, _], E[_, _], V, N, M](graph: G[E[V, N], N, M])(implicit
    numeric: Numeric[V],
    identifiable: Identifiable[N],
    graphType: Graph[G],
    edgeType: EdgeType[E]) extends DijkstraShortestPath[G, E, V, N, M](graph)
}
