package com.flowtick.graphs

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait NodeOps[N <: Node, E <: Edge[N]] {
  val node: N
}

trait DirectedNodeOps[N <: Node, E <: Edge[N]] { self: NodeOps[N, E] =>
  def ~>(target: N)(implicit graphBuilder: GraphBuilder[N, E]): N
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

  val nodes = mutable.HashSet.empty[N]
  val edges = mutable.HashSet.empty[E]

  val incoming: mutable.Map[N, ListBuffer[E]] = mutable.Map[N, mutable.ListBuffer[E]]()
  val outgoing: mutable.Map[N, ListBuffer[E]] = mutable.Map[N, mutable.ListBuffer[E]]()

  def addNode(node: N): nodes.type = nodes += node

  def addEdge(edge: E): edges.type = {
    def updateIncomingAndOutgoing(e: E) = {
      incoming.put(e.target, incoming.getOrElse(e.target, ListBuffer()) += e)
      outgoing.put(e.source, outgoing.getOrElse(e.source, ListBuffer()) += e)
    }

    edge match {
      case e: UndirectedEdge[N] =>
        updateIncomingAndOutgoing(edge)
        outgoing.put(edge.target, outgoing.getOrElse(edge.target, ListBuffer()) += edge)
        incoming.put(edge.source, incoming.getOrElse(edge.source, ListBuffer()) += edge)
      case _ => updateIncomingAndOutgoing(edge)
    }

    addNode(edge.source)
    addNode(edge.target)

    edges += edge
  }

  def build: Graph[N, E]
}