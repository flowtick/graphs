package com.flowtick.graphs

package object defaults {
  def n(id: String) = DefaultNode(id)

  implicit val defaultGraphBuilderProvider = () => new GraphBuilder[DefaultNode, Edge[DefaultNode]] {
    override def build: Graph[DefaultNode, Edge[DefaultNode]] = DefaultGraph(nodes.toSet, edges.toSet)
  }

  implicit val defaultDirectedGraphBuilderProvider = () => new GraphBuilder[DefaultNode, DirectedEdge[DefaultNode]] {
    override def build: Graph[DefaultNode, DirectedEdge[DefaultNode]] = DefaultGraph(nodes.toSet, edges.toSet)
  }

  implicit val defaultUndirectedGraphBuilderProvider = () => new GraphBuilder[DefaultNode, UndirectedEdge[DefaultNode]] {
    override def build: Graph[DefaultNode, UndirectedEdge[DefaultNode]] = DefaultGraph(nodes.toSet, edges.toSet)
  }

  implicit val defaultWeightedGraphBuilderProvider = () => new GraphBuilder[DefaultNode, WeightedEdge[Int, DefaultNode]] {
    override def build: Graph[DefaultNode, WeightedEdge[Int, DefaultNode]] = DefaultGraph(nodes.toSet, edges.toSet)
  }

  implicit class DefaultNodeOps[N <: Node](n: N) extends NodeOps[N, Edge[N]]
      with DirectedNodeOps[N, Edge[N]]
      with UndirectedNodeOps[N, Edge[N]] {
    val node = n

    override def ~>(target: N)(implicit graphBuilder: GraphBuilder[N, Edge[N]]): N = {
      graphBuilder.addEdge(DefaultDirectedEdge(None, node, target))
      target
    }
    override def ~>(target: N, label: String)(implicit graphBuilder: GraphBuilder[N, Edge[N]]): N = {
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
