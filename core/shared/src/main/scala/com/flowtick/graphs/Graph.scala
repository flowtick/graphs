package com.flowtick.graphs

trait Node {
  def id: String
}

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

object Graph {
  def create[N <: Node, E <: Edge[N]](block: GraphBuilder[N, E] => Unit)(implicit graphBuilderProvider: () => GraphBuilder[N, E]): Graph[N, E] =
    graphBuilderProvider()(block)
}