package com.flowtick.graphs

import com.flowtick.graphs.layout.{ Cell, GraphLayout, ShapeDefinition }

package object defaults extends {
  // #default_graph
  case class DefaultNode(id: String)
  case class DirectedEdge[N](source: N, target: Option[N]) extends Edge[DirectedEdge[N], N] {
    override def value: DirectedEdge[N] = this

    override def predecessors: Set[N] = Set(source)
    override def successors: Set[N] = Set(target).flatten
  }

  case class UndirectedEdge[N](source: N, target: Option[N]) extends Edge[UndirectedEdge[N], N] {
    override def value: UndirectedEdge[N] = this

    override def predecessors: Set[N] = Set(source)
    override def successors: Set[N] = Set(target).flatten

    override def incoming(node: N, graph: Graph[N, UndirectedEdge[N]]): Iterable[Edge[UndirectedEdge[N], N]] =
      super.incoming(node, graph) ++ super.outgoing(node, graph)

    override def outgoing(node: N, graph: Graph[N, UndirectedEdge[N]]): Iterable[Edge[UndirectedEdge[N], N]] =
      super.outgoing(node, graph) ++ super.incoming(node, graph)
  }

  case class WeightedEdge[E, N, V](edge: Edge[E, N], weight: V) extends Edge[WeightedEdge[E, N, V], N] {
    override def value: WeightedEdge[E, N, V] = this
    override def predecessors: Set[N] = edge.predecessors
    override def successors: Set[N] = edge.successors
  }

  object DefaultGraph {
    def create[N](tuples: Seq[(N, N)])(implicit
      identifiable: Identifiable[N],
      edgeBuilder: EdgeBuilder[N, DirectedEdge[N], (N, N)]): Graph[N, DirectedEdge[N]] =
      Graph.create[N, DirectedEdge[N], (N, N)](tuples: _*)

    def weighted[E, N, V](tuples: Seq[(V, (N, N))])(implicit
      identifiable: Identifiable[N],
      edgeBuilder: EdgeBuilder[N, WeightedEdge[E, N, V], (V, (N, N))]): Graph[N, WeightedEdge[E, N, V]] =
      Graph.create[N, WeightedEdge[E, N, V], (V, (N, N))](tuples: _*)
  }
  // #default_graph

  def n(id: String) = DefaultNode(id)

  implicit def identifiable: Identifiable[DefaultNode] = new Identifiable[DefaultNode] {
    override def id(node: DefaultNode): String = node.id
  }

  implicit def identifiableNumeric[N](implicit numeric: Numeric[N]): Identifiable[N] = new Identifiable[N] {
    override def id(node: N): String = numeric.toDouble(node).toString
  }

  implicit def defaultLayout: GraphLayout = new GraphLayout {
    override def layout[N, E](g: Graph[N, E], shape: N => Option[ShapeDefinition])(implicit
      identifiable: Identifiable[N],
      edgeLabel: Labeled[E, String]): collection.Map[String, Cell] = Map.empty
  }

  implicit def weight[E, N, V: Numeric]: Weighted[WeightedEdge[E, N, V], V] = new Weighted[WeightedEdge[E, N, V], V] {
    override def value(weighted: WeightedEdge[E, N, V]): V = weighted.weight
  }

  object directed {
    implicit def edgeBuilder[N]: EdgeBuilder[N, DirectedEdge[N], (N, N)] = new EdgeBuilder[N, DirectedEdge[N], (N, N)] {
      override def create(from: (N, N))(implicit identifiable: Identifiable[N]): DirectedEdge[N] = DirectedEdge(from._1, Some(from._2))
    }
  }

  object undirected {
    implicit def edgeBuilder[N]: EdgeBuilder[N, UndirectedEdge[N], (N, N)] = new EdgeBuilder[N, UndirectedEdge[N], (N, N)] {
      override def create(from: (N, N))(implicit identifiable: Identifiable[N]): UndirectedEdge[N] = UndirectedEdge(from._1, Some(from._2))
    }
  }

  implicit def singleNodeEdgeBuilder[N]: EdgeBuilder[N, DirectedEdge[N], N] = new EdgeBuilder[N, DirectedEdge[N], N] {
    override def create(from: N)(implicit identifiable: Identifiable[N]): DirectedEdge[N] = DirectedEdge(from, None)
  }

  implicit def edgeLabel[N]: Labeled[DirectedEdge[N], String] = new Labeled[DirectedEdge[N], String] {
    override def label(edge: DirectedEdge[N]): Option[String] = None
  }

  implicit def weightedEdgeBuilder[E, N, V, B](implicit edgeBuilder: EdgeBuilder[N, E, B]): EdgeBuilder[N, WeightedEdge[E, N, V], (V, B)] = new EdgeBuilder[N, WeightedEdge[E, N, V], (V, B)] {
    override def create(from: (V, B))(implicit identifiable: Identifiable[N]): Edge[WeightedEdge[E, N, V], N] =
      WeightedEdge(edgeBuilder.create(from._2), from._1)
  }

  implicit def weightedEdgeLabel[E, N, V]: Labeled[WeightedEdge[E, N, V], String] = new Labeled[WeightedEdge[E, N, V], String] {
    override def label(edge: WeightedEdge[E, N, V]): Option[String] = Some(edge.weight.toString)
  }
}
