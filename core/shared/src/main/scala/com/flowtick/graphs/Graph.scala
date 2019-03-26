package com.flowtick.graphs

/**
 * Type class to define an identifier for a node type as string.
 *
 * Similar to the Show type class but mainly used for serialisation purposes
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

final case class Edge[V, N](value: V, head: N, tail: N) {
  override def toString: String = {
    val valueString = if (value != Unit) s"[$value]" else ""

    s"$head --> $tail$valueString"
  }
}

// #graph
trait Graph[G[_, _, _]] {
  def value[V, N, M](graph: G[V, N, M]): M

  def edges[V, N, M](graph: G[V, N, M]): Iterable[Edge[V, N]]

  def nodes[V, N, M](graph: G[V, N, M]): Iterable[N]

  def incoming[V, N, M](graph: G[V, N, M]): scala.collection.Map[N, Iterable[Edge[V, N]]]

  def outgoing[V, N, M](graph: G[V, N, M]): scala.collection.Map[N, Iterable[Edge[V, N]]]

  def predecessors[V, N, M](node: N, graph: G[V, N, M]): Iterable[N] =
    incoming(graph).getOrElse(node, Iterable.empty).view.map(_.head)

  def successors[V, N, M](node: N, graph: G[V, N, M]): Iterable[N] =
    outgoing(graph).getOrElse(node, Iterable.empty).view.map(_.tail)
}
// #graph

trait GraphBuilder[G[_, _, _]] {
  def empty[V, N, M](meta: M): G[V, N, M] = build[V, N, M](meta, Iterable.empty, Iterable.empty, Map.empty, Map.empty)

  def withValue[V, N, M](value: M)(edges: Iterable[Edge[V, N]], nodes: Iterable[N]): G[V, N, M] =
    create(value, nodes, edges, None)

  def of[V, N, M](value: M, nodes: Option[Iterable[N]] = None)(edges: Edge[V, N]*): G[V, N, M] =
    create(value, nodes.getOrElse(Iterable.empty), edges, None)

  def from[V, N, M](edges: Iterable[Edge[V, N]], nodes: Option[Iterable[N]] = None): G[V, N, Unit] =
    create((), nodes.getOrElse(Iterable.empty), edges, None)

  def build[V, N, M](
    value: M,
    edges: Iterable[Edge[V, N]],
    nodes: Iterable[N],
    incoming: scala.collection.Map[N, Iterable[Edge[V, N]]],
    outgoing: scala.collection.Map[N, Iterable[Edge[V, N]]]): G[V, N, M]

  def ordering[V, N]: Ordering[Edge[V, N]]

  def create[V, N, M](
    value: M,
    nodes: Iterable[N],
    edges: Iterable[Edge[V, N]],
    customOrdering: Option[Ordering[Edge[V, N]]] = None): G[V, N, M] = {
    import scala.collection.mutable

    // order edges by ascending tail id, this is needed for stable algorithms
    implicit val edgeOrdering: Ordering[Edge[V, N]] = customOrdering.getOrElse(ordering)

    val incoming: mutable.HashMap[N, mutable.ListBuffer[Edge[V, N]]] = mutable.HashMap[N, mutable.ListBuffer[Edge[V, N]]]()
    val outgoing: mutable.HashMap[N, mutable.ListBuffer[Edge[V, N]]] = mutable.HashMap[N, mutable.ListBuffer[Edge[V, N]]]()

    val nodes = mutable.HashSet[N]()

    edges.foreach { edge =>
      val head = edge.head
      outgoing.put(head, outgoing.getOrElse(head, mutable.ListBuffer.empty) += edge)
      nodes += head

      val tail = edge.tail
      incoming.put(tail, incoming.getOrElse(tail, mutable.ListBuffer.empty) += edge)
      nodes += tail
    }

    build(value, edges, nodes, incoming, outgoing)
  }
}
