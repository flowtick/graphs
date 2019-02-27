package com.flowtick.graphs

/**
 * Type class to define an identifier for a node type as string.
 *
 * Mainly used for serialisation purposes
 *
 * @tparam T the node type
 */
trait Identifiable[T] {
  def id(node: T): String
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

trait Empty[T] {
  def empty: T
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

trait Graph[G[_, _, _], ET[_, _]] {
  def value[V, N, M](graph: G[ET[V, N], N, M]): M

  def edges[V, N, M](graph: G[ET[V, N], N, M])(implicit edgeType: EdgeType[ET]): Iterable[ET[V, N]]

  def nodes[V, N, M](graph: G[ET[V, N], N, M])(implicit edgeType: EdgeType[ET]): Iterable[N]

  def incoming[V, N, M](
    node: N,
    graph: G[ET[V, N], N, M])(implicit edgeType: EdgeType[ET], identifiable: Identifiable[N]): Iterable[ET[V, N]]

  def outgoing[V, N, M](
    node: N,
    graph: G[ET[V, N], N, M])(implicit edgeType: EdgeType[ET], identifiable: Identifiable[N]): Iterable[ET[V, N]]
}

trait GraphBuilder[G[_, _, _], ET[_, _]] {
  def empty[V, N, M](implicit emptyValue: Empty[M]): G[ET[V, N], N, M] = build[ET, V, N, M](emptyValue.empty, Iterable.empty, Iterable.empty, Map.empty, Map.empty)

  def withValue[V, N, M](value: M)(edges: Iterable[ET[V, N]])(implicit edgeType: EdgeType[ET], identifiable: Identifiable[N]): G[ET[V, N], N, M] =
    create(value, Iterable.empty, edges, None)

  def of[V, N, M](value: M)(edges: ET[V, N]*)(implicit
    edgeType: EdgeType[ET],
    identifiable: Identifiable[N]): G[ET[V, N], N, M] = create(value, Iterable.empty, edges, None)

  def from[V, N, M](edges: Iterable[ET[V, N]])(implicit edgeType: EdgeType[ET], identifiable: Identifiable[N]): G[ET[V, N], N, Unit] =
    create((), Iterable.empty, edges, None)

  def build[E[_, _], V, N, M](
    value: M,
    edges: Iterable[E[V, N]],
    nodes: Iterable[N],
    incoming: scala.collection.Map[N, Iterable[E[V, N]]],
    outgoing: scala.collection.Map[N, Iterable[E[V, N]]]): G[E[V, N], N, M]

  def create[V, N, M](
    value: M,
    nodes: Iterable[N],
    edges: Iterable[ET[V, N]],
    ordering: Option[Ordering[ET[V, N]]] = None)(implicit edgeType: EdgeType[ET], identifiable: Identifiable[N]): G[ET[V, N], N, M] = {
    import scala.collection.mutable

    // order edges by ascending tail id, this is needed for stable algorithms
    implicit val edgeOrdering: Ordering[ET[V, N]] = ordering.getOrElse(new Ordering[ET[V, N]] {
      override def compare(x: ET[V, N], y: ET[V, N]): Int = {
        val xid = identifiable.id(edgeType.head(x)) + identifiable.id(edgeType.tail(x))
        val yid = identifiable.id(edgeType.head(y)) + identifiable.id(edgeType.tail(y))

        xid.compareTo(yid)
      }
    })

    val incoming: mutable.Map[N, mutable.TreeSet[ET[V, N]]] = mutable.Map[N, mutable.TreeSet[ET[V, N]]]()
    val outgoing: mutable.Map[N, mutable.TreeSet[ET[V, N]]] = mutable.Map[N, mutable.TreeSet[ET[V, N]]]()

    val nodes = mutable.HashSet[N]()

    edges.foreach { edge =>
      val head = edgeType.head(edge)
      outgoing.put(head, outgoing.getOrElse(head, mutable.TreeSet.empty) += edge)
      nodes += head

      val tail = edgeType.tail(edge)
      incoming.put(tail, incoming.getOrElse(tail, mutable.TreeSet.empty) += edge)
      nodes += tail
    }

    build(value, edges, nodes, incoming, outgoing)
  }
}

object Graph {
  def apply[G[_, _, _], E[_, _]](implicit graph: Graph[G, E], edge: EdgeType[E]): Graph[G, E] = graph
}
