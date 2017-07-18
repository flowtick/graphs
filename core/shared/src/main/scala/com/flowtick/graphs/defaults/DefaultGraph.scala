package com.flowtick.graphs.defaults

import com.flowtick.graphs._

case class DefaultNode(id: String) extends Node
case class DefaultDirectedEdge[N <: Node](label: Option[String] = None, source: N, target: N) extends DirectedEdge[N]
case class DefaultUndirectedEdge[N <: Node](label: Option[String] = None, source: N, target: N) extends UndirectedEdge[N]

case class DefaultWeightedEdge[T: Numeric, N <: Node](
  value: T,
  override val label: Option[String] = None,
  override val source: N,
  override val target: N
) extends WeightedEdge[T, N](value, label, source, target)

case class DefaultGraph[N <: Node, E <: Edge[N]](graphBuilder: GraphBuilder[N, E]) extends AbstractGraph[N, E](graphBuilder)