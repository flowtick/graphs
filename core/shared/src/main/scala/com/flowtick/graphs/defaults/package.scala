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
  final case class DefaultGraph[V, N, M](
    private[defaults] val value: M,
    private[defaults] val edges: Iterable[Edge[V, N]],
    private[defaults] val nodes: Iterable[N],
    private[defaults] val incoming: scala.collection.Map[N, Iterable[Edge[V, N]]],
    private[defaults] val outgoing: scala.collection.Map[N, Iterable[Edge[V, N]]])

  private[defaults] class DefaultOrdering[V, N] extends Ordering[Edge[V, N]] {
    override def compare(x: Edge[V, N], y: Edge[V, N]): Int = {
      val xid = x.head.toString + x.tail.toString
      val yid = y.head.toString + y.tail.toString

      xid.compareTo(yid)
    }
  }

  private[defaults] trait DefaultGraphBase extends Graph[DefaultGraph] with GraphBuilder[DefaultGraph] {
    override def edges[V, N, M](graph: DefaultGraph[V, N, M]): Iterable[Edge[V, N]] =
      graph.edges

    override def incoming[V, N, M](
      graph: DefaultGraph[V, N, M]): scala.collection.Map[N, Iterable[Edge[V, N]]] =
      graph.incoming

    override def outgoing[V, N, M](
      graph: DefaultGraph[V, N, M]): scala.collection.Map[N, Iterable[Edge[V, N]]] =
      graph.outgoing

    override def nodes[V, N, M](graph: DefaultGraph[V, N, M]): Iterable[N] =
      graph.nodes

    override def build[V, N, M](
      value: M,
      edges: Iterable[Edge[V, N]],
      nodes: Iterable[N],
      incoming: scala.collection.Map[N, Iterable[Edge[V, N]]],
      outgoing: scala.collection.Map[N, Iterable[Edge[V, N]]]): DefaultGraph[V, N, M] = DefaultGraph(value, edges, nodes, incoming, outgoing)

    override def value[V, N, M](graph: DefaultGraph[V, N, M]): M = graph.value

    override def ordering[V, N]: Ordering[Edge[V, N]] = new DefaultOrdering
  }

  private[defaults] class DirectedGraph extends DefaultGraphBase

  private[defaults] class UndirectedGraph extends DefaultGraphBase {
    override def build[V, N, M](
      value: M,
      edges: Iterable[Edge[V, N]],
      nodes: Iterable[N],
      incoming: collection.Map[N, Iterable[Edge[V, N]]],
      outgoing: collection.Map[N, Iterable[Edge[V, N]]]): DefaultGraph[V, N, M] = {
      val mergedEdgesByNode = (incoming foldLeft outgoing)(
        (acc, v) => acc + (v._1 -> (v._2 ++ acc.getOrElse(v._1, Iterable.empty))))

      super.build(value, edges, nodes, mergedEdgesByNode, mergedEdgesByNode)
    }
  }

  // #default_graph

  def n[X](value: X, label: Option[String] = None) = Node[X](value, label)

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

  implicit def labeledEdge[V, N](implicit labeled: Labeled[V, String]): Labeled[Edge[V, N], String] = new Labeled[Edge[V, N], String] {
    override def label(e: Edge[V, N]): Option[String] = labeled.label(e.value)
  }

  object directed {
    implicit class DirectedEdgeBuilder[X](node: Node[X]) {
      def -->[V](value: V, to: Node[X]): Edge[V, X] = Edge[V, X](value, node.value, to.value)
      def -->[V](to: Node[X]): Edge[Unit, X] = Edge[Unit, X]((), node.value, to.value)
    }

    implicit val directedGraph: DirectedGraph = new DirectedGraph
  }

  object undirected {
    implicit class UndirectedEdgeBuilder[X](node: Node[X]) {
      def ~~~[V](value: V, to: Node[X]): Edge[V, X] = Edge[V, X](value, node.value, to.value)
      def ~~~[V](to: Node[X]): Edge[Unit, X] = Edge[Unit, X]((), node.value, to.value)
    }

    implicit val undirectedGraph: UndirectedGraph = new UndirectedGraph
  }

}
