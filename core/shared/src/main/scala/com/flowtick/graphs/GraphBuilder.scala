package com.flowtick.graphs

import scala.collection.mutable

trait NodeOps[N <: Node, E <: Edge[N]] {
  val node: N
}

trait DirectedNodeOps[N <: Node, E <: Edge[N]] { self: NodeOps[N, E] =>
  def ~>(target: N)(implicit graphBuilder: GraphBuilder[N, E]): N
  def ~>(target: N, label: String)(implicit graphBuilder: GraphBuilder[N, E]): N
}

trait UndirectedNodeOps[N <: Node, E <: Edge[N]] { self: NodeOps[N, E] =>
  def ~(target: N)(implicit graphBuilder: GraphBuilder[N, E]): N
  def ~[T, WE <: WeightedEdge[T, N]](value: T, target: N)(implicit ordering: Numeric[T], graphBuilder: GraphBuilder[N, WeightedEdge[T, N]]): N
  def ~[T, WE <: WeightedEdge[T, N]](value: T, target: N, label: String)(implicit ordering: Numeric[T], graphBuilder: GraphBuilder[N, WeightedEdge[T, N]]): N
}

trait GraphBuilder[N <: Node, E <: Edge[N]] {
  def apply(block: GraphBuilder[N, E] => Unit): Graph[N, E] = {
    block(this)
    build
  }

  protected val nodes = mutable.HashSet.empty[N]
  protected val edges = mutable.HashSet.empty[E]

  def addNode(node: N) = nodes += node
  def addEdge(edge: E) = {
    addNode(edge.source)
    addNode(edge.target)
    edges += edge
  }

  def build: Graph[N, E]
}