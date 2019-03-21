package com.flowtick.graphs

/**
 * Type class to define an identifier for a node type as string.
 *
 * Mainly used for serialisation purposes
 *
 * @tparam T the node type
 */
trait Identifiable[-T] {
  def id(node: T): String
}

object Identifiable {
  /** creates an instance of [[Identifiable]] using the provided function */
  def identify[A](f: A => String): Identifiable[A] = new Identifiable[A] {
    def id(a: A): String = f(a)
  }
}

/**
 * Type class to define an optional label for a node type as string.
 *
 * @tparam E edge value type
 * @tparam L label type
 */
trait Labeled[E, L] {
  def label(edge: E): Option[L]
}

trait EdgeType[ET[_, _]] {
  def apply[V, N](
    value: V,
    head: N,
    tail: N): ET[V, N]

  def value[V, N](edge: ET[V, N]): V

  def head[V, N](edge: ET[V, N]): N
  def tail[V, N](edge: ET[V, N]): N
}

// #graph
trait Graph[G[_, _, _], ET[_, _]] {
  def value[V, N, M](graph: G[ET[V, N], N, M]): M

  def edges[V, N, M](graph: G[ET[V, N], N, M]): Iterable[ET[V, N]]

  def nodes[V, N, M](graph: G[ET[V, N], N, M]): Iterable[N]

  def incoming[V, N, M](graph: G[ET[V, N], N, M]): scala.collection.Map[N, Iterable[ET[V, N]]]

  def outgoing[V, N, M](graph: G[ET[V, N], N, M]): scala.collection.Map[N, Iterable[ET[V, N]]]

  def predecessors[V, N, M](node: N, graph: G[ET[V, N], N, M])(implicit edgeType: EdgeType[ET]): Iterable[N] =
    incoming(graph).getOrElse(node, Iterable.empty).view.map(edgeType.head)

  def successors[V, N, M](node: N, graph: G[ET[V, N], N, M])(implicit edgeType: EdgeType[ET]): Iterable[N] =
    outgoing(graph).getOrElse(node, Iterable.empty).view.map(edgeType.tail)
}
// #graph

trait GraphBuilder[G[_, _, _], ET[_, _]] {
  def empty[V, N, M](meta: M): G[ET[V, N], N, M] = build[V, N, M](meta, Iterable.empty, Iterable.empty, Map.empty, Map.empty)

  def withValue[V, N, M](value: M)(edges: Iterable[ET[V, N]], nodes: Iterable[N])(implicit edgeType: EdgeType[ET]): G[ET[V, N], N, M] =
    create(value, nodes, edges, None)

  def of[V, N, M](value: M, nodes: Option[Iterable[N]] = None)(edges: ET[V, N]*)(implicit edgeType: EdgeType[ET]): G[ET[V, N], N, M] = create(value, nodes.getOrElse(Iterable.empty), edges, None)

  def from[V, N, M](edges: Iterable[ET[V, N]], nodes: Option[Iterable[N]] = None)(implicit edgeType: EdgeType[ET]): G[ET[V, N], N, Unit] =
    create((), nodes.getOrElse(Iterable.empty), edges, None)

  def build[V, N, M](
    value: M,
    edges: Iterable[ET[V, N]],
    nodes: Iterable[N],
    incoming: scala.collection.Map[N, Iterable[ET[V, N]]],
    outgoing: scala.collection.Map[N, Iterable[ET[V, N]]]): G[ET[V, N], N, M]

  def create[V, N, M](
    value: M,
    nodes: Iterable[N],
    edges: Iterable[ET[V, N]],
    ordering: Option[Ordering[ET[V, N]]] = None)(implicit edgeType: EdgeType[ET]): G[ET[V, N], N, M] = {
    import scala.collection.mutable

    // order edges by ascending tail id, this is needed for stable algorithms
    implicit val edgeOrdering: Ordering[ET[V, N]] = ordering.getOrElse(new Ordering[ET[V, N]] {
      override def compare(x: ET[V, N], y: ET[V, N]): Int = {
        val xid = edgeType.head(x).toString + edgeType.tail(x).toString
        val yid = edgeType.head(y).toString + edgeType.tail(y).toString

        xid.compareTo(yid)
      }
    })

    val incoming: mutable.HashMap[N, mutable.ListBuffer[ET[V, N]]] = mutable.HashMap[N, mutable.ListBuffer[ET[V, N]]]()
    val outgoing: mutable.HashMap[N, mutable.ListBuffer[ET[V, N]]] = mutable.HashMap[N, mutable.ListBuffer[ET[V, N]]]()

    val nodes = mutable.HashSet[N]()

    edges.foreach { edge =>
      val head = edgeType.head(edge)
      outgoing.put(head, outgoing.getOrElse(head, mutable.ListBuffer.empty) += edge)
      nodes += head

      val tail = edgeType.tail(edge)
      incoming.put(tail, incoming.getOrElse(tail, mutable.ListBuffer.empty) += edge)
      nodes += tail
    }

    build(value, edges, nodes, incoming, outgoing)
  }
}

object Graph {
  def apply[G[_, _, _], E[_, _]](implicit graph: Graph[G, E], edge: EdgeType[E]): Graph[G, E] = graph
}
