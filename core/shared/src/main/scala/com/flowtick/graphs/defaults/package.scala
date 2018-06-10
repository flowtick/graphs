package com.flowtick.graphs

import com.flowtick.graphs.layout.{ Cell, GraphLayout, ShapeDefinition }

package object defaults extends {
  // #default_graph
  case class DefaultNode(id: String)
  case class DefaultEdge[N](source: N, target: Option[N], directed: Boolean = true) extends Edge[DefaultEdge[N], N] {
    override def value: DefaultEdge[N] = this

    override def predecessors: Set[N] = if (directed) Set(source) else Set(source) ++ Set(target).flatten
    override def successors: Set[N] = if (directed) Set(target).flatten else Set(target).flatten ++ Set(source)
  }

  case class WeightedEdge[E, N, V](edge: Edge[E, N], weight: V) extends Edge[WeightedEdge[E, N, V], N] {
    override def value: WeightedEdge[E, N, V] = this
    override def predecessors: Set[N] = edge.predecessors
    override def successors: Set[N] = edge.successors
  }

  object DefaultGraph {
    def create[N](tuples: Seq[(N, N)])(implicit
      identifiable: Identifiable[N],
      edgeBuilder: EdgeBuilder[N, DefaultEdge[N], (N, N)]): Graph[N, DefaultEdge[N]] =
      Graph.create[N, DefaultEdge[N], (N, N)](tuples: _*)

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
    implicit def edgeBuilder[N]: EdgeBuilder[N, DefaultEdge[N], (N, N)] = new EdgeBuilder[N, DefaultEdge[N], (N, N)] {
      override def create(from: (N, N))(implicit identifiable: Identifiable[N]): DefaultEdge[N] = DefaultEdge(from._1, Some(from._2))
    }
  }

  object undirected {
    implicit def edgeBuilder[N]: EdgeBuilder[N, DefaultEdge[N], (N, N)] = new EdgeBuilder[N, DefaultEdge[N], (N, N)] {
      override def create(from: (N, N))(implicit identifiable: Identifiable[N]): DefaultEdge[N] = DefaultEdge(from._1, Some(from._2), directed = false)
    }
  }

  implicit def singleNodeEdgeBuilder[N]: EdgeBuilder[N, DefaultEdge[N], N] = new EdgeBuilder[N, DefaultEdge[N], N] {
    override def create(from: N)(implicit identifiable: Identifiable[N]): DefaultEdge[N] = DefaultEdge(from, None)
  }

  implicit def edgeLabel[N]: Labeled[DefaultEdge[N], String] = new Labeled[DefaultEdge[N], String] {
    override def label(edge: DefaultEdge[N]): Option[String] = None
  }

  implicit def weightedEdgeBuilder[E, N, V, B](implicit edgeBuilder: EdgeBuilder[N, E, B]): EdgeBuilder[N, WeightedEdge[E, N, V], (V, B)] = new EdgeBuilder[N, WeightedEdge[E, N, V], (V, B)] {
    override def create(from: (V, B))(implicit identifiable: Identifiable[N]): Edge[WeightedEdge[E, N, V], N] =
      WeightedEdge(edgeBuilder.create(from._2), from._1)
  }

  implicit def weightedEdgeLabel[E, N, V]: Labeled[WeightedEdge[E, N, V], String] = new Labeled[WeightedEdge[E, N, V], String] {
    override def label(edge: WeightedEdge[E, N, V]): Option[String] = Some(edge.weight.toString)
  }
}
