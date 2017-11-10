package com.flowtick.graphs

import com.flowtick.graphs
import com.flowtick.graphs.layout.{Cell, GraphLayout, ShapeDefinition}

package object defaults {
  def n(id: String) = DefaultNode(id)

  implicit def identifiable: Identifiable[DefaultNode] = new Identifiable[DefaultNode] {
    override def id(node: DefaultNode): String = node.id
  }

  implicit class DefaultLayout[N <: Node, Edge[N]](graph: Graph[N, Edge]) {
    def layout: GraphLayout = new GraphLayout {
      override def layout[N <: Node, E <: graphs.Edge[N]](g: Graph[N, E], shape: N => Option[ShapeDefinition])(implicit identifiable: Identifiable[N]): collection.Map[String, Cell] = ???
    }
  }

  implicit class DefaultNodeOps[N <: Node](n: N) extends NodeOps[N, Edge[N]]
    with DirectedNodeOps[N, Edge[N]]
    with UndirectedNodeOps[N, Edge[N]] {
    val node: N = n

    override def ~>(target: N)(implicit graphBuilder: GraphBuilder[N, Edge[N]]): N = {
      graphBuilder.addEdge(DefaultDirectedEdge(None, node, target))
      target
    }

    def ~>(target: N, label: String)(implicit graphBuilder: GraphBuilder[N, Edge[N]]): N = {
      graphBuilder.addEdge(DefaultDirectedEdge(Some(label), node, target))
      target
    }

    override def ~(target: N)(implicit graphBuilder: GraphBuilder[N, Edge[N]]): N = {
      graphBuilder.addEdge(DefaultUndirectedEdge(None, node, target))
      target
    }

    override def ~[T, WE <: WeightedEdge[T, N]](value: T, target: N)(implicit ordering: Numeric[T], graphBuilder: GraphBuilder[N, WeightedEdge[T, N]]): N = {
      graphBuilder.addEdge(DefaultWeightedEdge(value, None, node, target))
      target
    }
    override def ~[T, WE <: WeightedEdge[T, N]](value: T, target: N, label: String)(implicit ordering: Numeric[T], graphBuilder: GraphBuilder[N, WeightedEdge[T, N]]): N = {
      graphBuilder.addEdge(DefaultWeightedEdge(value, Some(label), node, target))
      target
    }
  }
}
