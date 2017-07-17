package com.flowtick.graphs

package object algorithm {
  implicit class Bfs[N <: Node, E <: Edge[N]](graph: Graph[N, E]) extends BreadthFirstSearch[N, E](graph)
  implicit class Dfs[N <: Node, E <: Edge[N]](graph: Graph[N, E]) extends DepthFirstSearch[N, E](graph)
  implicit class Dijkstra[T: Numeric, N <: Node, E <: WeightedEdge[T, N]](graph: Graph[N, WeightedEdge[T, N]]) extends DijkstraShortestPath[T, N, E](graph)
  implicit class Topological[N <: Node, E <: DirectedEdge[N]](graph: Graph[N, E]) extends TopologicalSort[N, E](graph)
}
