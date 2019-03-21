package com.flowtick.graphs

package object defaults {
  // #default_graph
  final case class Node[X](value: X, label: Option[String] = None) {
    override def toString: String = s"${value.toString}${label.map(" (" + _ + ")").getOrElse("")}"
  }

  /**
   * case class holding the values of a default graph
   *
   * the fields are private since we want the type class instance to be used for access
   *
   * @param edges
   * @param nodes
   * @param incoming
   * @param outgoing
   * @tparam E
   * @tparam N
   */
  final case class DefaultGraph[E, N, M](
    private[defaults] val value: M,
    private[defaults] val edges: Iterable[E],
    private[defaults] val nodes: Iterable[N],
    private[defaults] val incoming: scala.collection.Map[N, Iterable[E]],
    private[defaults] val outgoing: scala.collection.Map[N, Iterable[E]])

  final case class Edge[V, N](value: V, head: N, tail: N) {
    override def toString: String = {
      val valueString = if (value != Unit) s"[$value]" else ""

      s"$head --> $tail$valueString"
    }
  }

  class DefaultGraphInstance[G[_, _, _]] extends Graph[DefaultGraph] with GraphBuilder[DefaultGraph] {
    override def edges[ET[_, _], V, N, M](graph: DefaultGraph[ET[V, N], N, M]): Iterable[ET[V, N]] =
      graph.edges

    override def incoming[ET[_, _], V, N, M](
      graph: DefaultGraph[ET[V, N], N, M]): scala.collection.Map[N, Iterable[ET[V, N]]] =
      graph.incoming

    override def outgoing[ET[_, _], V, N, M](
      graph: DefaultGraph[ET[V, N], N, M]): scala.collection.Map[N, Iterable[ET[V, N]]] =
      graph.outgoing

    override def nodes[ET[_, _], V, N, M](graph: DefaultGraph[ET[V, N], N, M]): Iterable[N] =
      graph.nodes

    override def build[ET[_, _], V, N, M](
      value: M,
      edges: Iterable[ET[V, N]],
      nodes: Iterable[N],
      incoming: scala.collection.Map[N, Iterable[ET[V, N]]],
      outgoing: scala.collection.Map[N, Iterable[ET[V, N]]]): DefaultGraph[ET[V, N], N, M] = DefaultGraph(value, edges, nodes, incoming, outgoing)

    override def value[ET[_, _], V, N, M](graph: DefaultGraph[ET[V, N], N, M]): M = graph.value
  }

  // #default_graph

  def n[X](value: X, label: Option[String] = None) = Node[X](value, label)

  implicit def defaultGraph[G[_, _, _]] = new DefaultGraphInstance[G]

  implicit def identifiableString[X]: Identifiable[String] = new Identifiable[String] {
    override def id(string: String): String = string
  }

  implicit def identifiableNumeric[N](implicit numeric: Numeric[N]): Identifiable[N] = new Identifiable[N] {
    override def id(number: N): String = numeric.toDouble(number).toString
  }

  implicit def unitLabel: Labeled[Unit, String] = new Labeled[Unit, String] {
    override def label(edge: Unit): Option[String] = None
  }

  implicit def stringLabel: Labeled[String, String] = new Labeled[String, String] {
    override def label(string: String): Option[String] = Some(string)
  }

  implicit def numericLabel[T](implicit numeric: Numeric[T]): Labeled[T, String] = new Labeled[T, String] {
    override def label(number: T): Option[String] = Some(numeric.toDouble(number).toString)
  }

  implicit def labeledEdge[E[_, _], V, N](implicit edge: EdgeType[E], labeled: Labeled[V, String]): Labeled[E[V, N], String] = new Labeled[E[V, N], String] {
    override def label(e: E[V, N]): Option[String] = labeled.label(edge.value(e))
  }

  object directed {
    implicit object edge extends EdgeType[Edge] {
      override def apply[V, N](
        value: V,
        head: N,
        tail: N): Edge[V, N] = Edge(value, head, tail)

      override def value[V, N](edge: Edge[V, N]): V = edge.value

      override def head[V, N](edge: Edge[V, N]): N = edge.head
      override def tail[V, N](edge: Edge[V, N]): N = edge.tail
    }

    implicit class DirectedEdgeBuilder[X](node: Node[X]) {
      def -->[V](value: V, to: Node[X]): Edge[V, X] = Edge[V, X](value, node.value, to.value)
      def -->[V](to: Node[X]): Edge[Unit, X] = Edge[Unit, X]((), node.value, to.value)
    }
  }

}
