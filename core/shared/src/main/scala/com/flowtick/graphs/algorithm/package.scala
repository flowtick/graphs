package com.flowtick.graphs

package object algorithm {
  implicit class GraphOps[N, E](graph: Graph[N, E]) {
    def bfs(startNode: N) = new BreadthFirstSearch[N, E](Seq(startNode), graph)
    def dfs(startNode: N) = new DepthFirstSearch[N, E](Seq(startNode), graph)
    def topologicalSort: List[N] = new TopologicalSort[N, E](graph).sort
  }

  implicit class Dijkstra[T, N, E](graph: Graph[N, E]) extends DijkstraShortestPath[T, N, E](graph)
}
