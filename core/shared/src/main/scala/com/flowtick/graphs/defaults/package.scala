package com.flowtick.graphs

import com.flowtick.graphs.layout.{ Cell, GraphLayout, ShapeDefinition }

package object defaults extends {
  // #default_graph
  case class DefaultNode(id: String)
  case class DefaultEdge[N](source: N, target: Option[N])
  case class WeightedEdge[E, V](edge: E, weight: V)

  case class DefaultGraph[N, E](edges: Set[E])(implicit val edgeType: Edge[E, N]) extends AbstractGraph[N, E]

  object DefaultGraph {
    def create[N](tuples: Seq[(N, N)])(implicit
      identifiable: Identifiable[N],
      edge: Edge[DefaultEdge[N], N],
      edgeBuilder: EdgeBuilder[N, DefaultEdge[N], (N, N)],
      graphBuilder: GraphBuilder[N, DefaultEdge[N], DefaultGraph[N, DefaultEdge[N]], Unit]): DefaultGraph[N, DefaultEdge[N]] =
      Graph.create[N, DefaultEdge[N], DefaultGraph[N, DefaultEdge[N]], (N, N), Unit](tuples: _*)()

    def weighted[E, N, V](tuples: Seq[(V, (N, N))])(implicit
      identifiable: Identifiable[N],
      edge: Edge[WeightedEdge[E, V], N],
      edgeBuilder: EdgeBuilder[N, WeightedEdge[E, V], (V, (N, N))],
      graphBuilder: GraphBuilder[N, WeightedEdge[E, V], DefaultGraph[N, WeightedEdge[E, V]], Unit]): DefaultGraph[N, WeightedEdge[E, V]] =
      Graph.create[N, WeightedEdge[E, V], DefaultGraph[N, WeightedEdge[E, V]], (V, (N, N)), Unit](tuples: _*)()
  }
  // #default_graph

  def n(id: String) = DefaultNode(id)

  implicit def identifiable: Identifiable[DefaultNode] = new Identifiable[DefaultNode] {
    override def id(node: DefaultNode): String = node.id
  }

  implicit def defaultLayout: GraphLayout = new GraphLayout {
    override def layout[N, E](g: Graph[N, E], shape: N => Option[ShapeDefinition])(implicit
      identifiable: Identifiable[N],
      edgeLabel: Labeled[E, String]): collection.Map[String, Cell] = Map.empty
  }

  object directed {
    implicit def directedEdge[N]: Edge[DefaultEdge[N], N] = new Edge[DefaultEdge[N], N] {
      override def first(edge: DefaultEdge[N]): N = edge.source
      override def second(edge: DefaultEdge[N]): Option[N] = edge.target
    }
  }

  object undirected {
    implicit def undirectedEdge[N]: Edge[DefaultEdge[N], N] = new Edge[DefaultEdge[N], N] {
      override def first(edge: DefaultEdge[N]): N = edge.source
      override def second(edge: DefaultEdge[N]): Option[N] = edge.target

      override def incoming(node: N, graph: Graph[N, DefaultEdge[N]]): Iterable[DefaultEdge[N]] =
        super.incoming(node, graph) ++ super.outgoing(node, graph)

      override def outgoing(node: N, graph: Graph[N, DefaultEdge[N]]): Iterable[DefaultEdge[N]] =
        super.incoming(node, graph) ++ super.outgoing(node, graph)
    }
  }

  implicit def weightedEdge[E, N, V](implicit edgeType: Edge[E, N]): Edge[WeightedEdge[E, V], N] = new Edge[WeightedEdge[E, V], N] {
    override def first(weightedEdge: WeightedEdge[E, V]): N = edgeType.first(weightedEdge.edge)
    override def second(weightedEdge: WeightedEdge[E, V]): Option[N] = edgeType.second(weightedEdge.edge)
  }

  implicit def weight[N, V: Numeric]: Weighted[WeightedEdge[N, V], V] = new Weighted[WeightedEdge[N, V], V] {
    override def value(weighted: WeightedEdge[N, V]): V = weighted.weight
  }

  implicit def graphBuilder[N, E](implicit edge: Edge[E, N]): GraphBuilder[N, E, DefaultGraph[N, E], Unit] = new GraphBuilder[N, E, DefaultGraph[N, E], Unit] {
    override def create(edges: Set[E], nodes: Set[N], param: Unit): DefaultGraph[N, E] = DefaultGraph[N, E](edges)
  }

  implicit def edgeBuilder[N]: EdgeBuilder[N, DefaultEdge[N], (N, N)] = new EdgeBuilder[N, DefaultEdge[N], (N, N)] {
    override def create(from: (N, N))(implicit identifiable: Identifiable[N]): DefaultEdge[N] = DefaultEdge(from._1, Some(from._2))
  }

  implicit def singleNodeEdgeBuilder[N]: EdgeBuilder[N, DefaultEdge[N], N] = new EdgeBuilder[N, DefaultEdge[N], N] {
    override def create(from: N)(implicit identifiable: Identifiable[N]): DefaultEdge[N] = DefaultEdge(from, None)
  }

  implicit def edgeLabel[N]: Labeled[DefaultEdge[N], String] = new Labeled[DefaultEdge[N], String] {
    override def label(edge: DefaultEdge[N]): Option[String] = None
  }

  implicit def weightedEdgeBuilder[E, N, V, B](implicit edgeBuilder: EdgeBuilder[N, E, B]): EdgeBuilder[N, WeightedEdge[E, V], (V, B)] = new EdgeBuilder[N, WeightedEdge[E, V], (V, B)] {
    override def create(from: (V, B))(implicit identifiable: Identifiable[N]): WeightedEdge[E, V] =
      WeightedEdge(edgeBuilder.create(from._2), from._1)
  }

  implicit def weightedEdgeLabel[N, V]: Labeled[WeightedEdge[N, V], String] = new Labeled[WeightedEdge[N, V], String] {
    override def label(edge: WeightedEdge[N, V]): Option[String] = Some(edge.weight.toString)
  }
}
