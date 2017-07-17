package com.flowtick.graphs.defaults

import com.flowtick.graphs._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class DefaultNode(id: String) extends Node
case class DefaultDirectedEdge[N <: Node](label: Option[String] = None, source: N, target: N) extends DirectedEdge[N]
case class DefaultUndirectedEdge[N <: Node](label: Option[String] = None, source: N, target: N) extends UndirectedEdge[N]

case class DefaultWeightedEdge[T: Numeric, N <: Node](
  value: T,
  override val label: Option[String] = None,
  override val source: N,
  override val target: N
) extends WeightedEdge[T, N](value, label, source, target)

case class DefaultGraph[N <: Node, E <: Edge[N]](nodes: Set[N], edges: Set[E]) extends Graph[N, E] {
  val (incomingMap, outgoingMap) = {
    val incoming = mutable.Map[N, mutable.ListBuffer[E]]()
    val outgoing = mutable.Map[N, mutable.ListBuffer[E]]()

    def addEdge(e: E) = {
      outgoing.put(e.source, outgoing.getOrElse(e.source, ListBuffer()) += e)
      incoming.put(e.target, incoming.getOrElse(e.target, ListBuffer()) += e)
    }

    edges.map {
      case e: E with UndirectedEdge[N] =>
        addEdge(e)
        outgoing.put(e.target, outgoing.getOrElse(e.target, ListBuffer()) += e)
        incoming.put(e.source, incoming.getOrElse(e.source, ListBuffer()) += e)
      case e: E with DirectedEdge[N] => addEdge(e)
    }

    (incoming, outgoing)
  }

  def incoming(node: N): Iterable[E] = incomingMap.getOrElse(node, Iterable.empty)
  def outgoing(node: N): Iterable[E] = outgoingMap.getOrElse(node, Iterable.empty)
}