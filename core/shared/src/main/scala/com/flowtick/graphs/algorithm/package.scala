package com.flowtick.graphs

package object algorithm {
  implicit class GraphOps[N <: Node, E <: Edge[N]](graph: Graph[N, E]) {
    def bfs = new BreadthFirstSearch[N, E](graph)
    def dfs = new DepthFirstSearch[N, E](graph)
    def topologicalSort = new TopologicalSort[N, E](graph).sort
  }
  implicit class Dijkstra[T: Numeric, N <: Node, E <: WeightedEdge[T, N]](graph: Graph[N, WeightedEdge[T, N]]) extends DijkstraShortestPath[T, N, E](graph)
}
