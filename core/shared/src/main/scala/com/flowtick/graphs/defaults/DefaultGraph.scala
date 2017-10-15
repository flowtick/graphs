package com.flowtick.graphs.defaults

import com.flowtick.graphs._

// #default_graph
case class DefaultNode(id: String) extends Node
case class DefaultDirectedEdge[N <: Node](label: Option[String] = None, source: N, target: N) extends DirectedEdge[N]
case class DefaultUndirectedEdge[N <: Node](label: Option[String] = None, source: N, target: N) extends UndirectedEdge[N]

case class DefaultWeightedEdge[T: Numeric, N <: Node](
  value: T,
  override val label: Option[String] = None,
  override val source: N,
  override val target: N) extends WeightedUndirectedEdge[T, N](value, label, source, target)

case class DefaultGraph[N <: Node, E <: Edge[N]](graphBuilder: GraphBuilder[N, E]) extends AbstractGraph[N, E](graphBuilder)
// #default_graph

object DefaultGraph {
  class DefaultGraphBuilder extends GraphBuilder[DefaultNode, Edge[DefaultNode]] {
    override def build: DefaultGraph[DefaultNode, Edge[DefaultNode]] = DefaultGraph(this)
  }

  class DirectedGraphBuilder extends GraphBuilder[DefaultNode, DirectedEdge[DefaultNode]] {
    override def build: DefaultGraph[DefaultNode, DirectedEdge[DefaultNode]] = DefaultGraph(this)
  }

  class UndirectedGraphBuilder extends GraphBuilder[DefaultNode, UndirectedEdge[DefaultNode]] {
    override def build: DefaultGraph[DefaultNode, UndirectedEdge[DefaultNode]] = DefaultGraph(this)
  }

  class WeightedGraphBuilder extends GraphBuilder[DefaultNode, WeightedEdge[Int, DefaultNode]] {
    override def build: DefaultGraph[DefaultNode, WeightedEdge[Int, DefaultNode]] = DefaultGraph(this)
  }

  def create(block: DefaultGraphBuilder => Any): DefaultGraph[DefaultNode, Edge[DefaultNode]] = {
    val builder = new DefaultGraphBuilder
    block.apply(builder)
    builder.build
  }

  def directed(block: DirectedGraphBuilder => Any): DefaultGraph[DefaultNode, DirectedEdge[DefaultNode]] = {
    val builder = new DirectedGraphBuilder
    block.apply(builder)
    builder.build
  }

  def undirected(block: UndirectedGraphBuilder => Any): DefaultGraph[DefaultNode, UndirectedEdge[DefaultNode]] = {
    val builder = new UndirectedGraphBuilder
    block.apply(builder)
    builder.build
  }

  def weighted(block: WeightedGraphBuilder => Any): DefaultGraph[DefaultNode, WeightedEdge[Int, DefaultNode]] = {
    val builder = new WeightedGraphBuilder
    block.apply(builder)
    builder.build
  }
}