package com.flowtick.graphs

trait Node

trait Edge[N <: Node] {
  def label: Option[String]
  def source: N
  def target: N
}

trait DirectedEdge[N <: Node] extends Edge[N]
trait UndirectedEdge[N <: Node] extends Edge[N]

abstract class WeightedEdge[T: Numeric, N <: Node](
  val weight: T,
  val label: Option[String] = None,
  val source: N,
  val target: N
) extends UndirectedEdge[N]

trait Graph[N <: Node, E <: Edge[N]] {
  def nodes: Set[N]
  def edges: Set[E]

  def incoming(node: N): Iterable[E]
  def outgoing(node: N): Iterable[E]
}

abstract class AbstractGraph[N <: Node, E <: Edge[N]](graphBuilder: GraphBuilder[N, E]) extends Graph[N, E] {
  val edges: Set[E] = graphBuilder.edges.toSet
  val nodes: Set[N] = graphBuilder.nodes.toSet

  def incoming(node: N): Iterable[E] = graphBuilder.incoming.getOrElse(node, Iterable.empty)
  def outgoing(node: N): Iterable[E] = graphBuilder.outgoing.getOrElse(node, Iterable.empty)
}
