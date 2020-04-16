package com.flowtick.graphs

/**
 * Type class to define an identifier for a value
 *
 * Similar to the Show type class but mainly used for serialisation purposes
 *
 * @tparam T the node type
 */
trait Identifiable[T, I] {
  def apply(value: T): I
}

object Identifiable {
  /** creates an instance of [[Identifiable]] using the provided function */
  def identify[T, I](f: T => I): Identifiable[T, I] = (a: T) => f(a)

  def identity[T]: Identifiable[T, T] = id => id
}

/**
 * Type class to define a label
 *
 * @tparam E edge value type
 * @tparam L label type
 */
trait Labeled[E, L] {
  def apply(edge: E): L
}

object Labeled {
  /** creates an instance of [[Labeled]] using the provided function */
  def label[T, I](f: T => I): Labeled[T, I] = (a: T) => f(a)

  def identity[T]: Labeled[T, T] = label => label
}

final case class Edge[E, N](value: E, from: N, to: N) {
  override def toString: String = s"$from --> $to"
}

final case class NodeContext[N, E](node: N, incoming: List[Edge[E, N]] = List.empty, outgoing: List[Edge[E, N]] = List.empty)

final case class GraphInstance[E, N, M](meta: M,
                                        contexts: scala.collection.Map[N, NodeContext[N, E]] = scala.collection.Map.empty[N, NodeContext[N, E]],
                                        edges: scala.collection.Set[Edge[E, N]] = scala.collection.Set.empty[Edge[E, N]]) extends Graph[E, N, M] {
  def +(edge: Edge[E, N]): Graph[E, N, M] = {
    val newFromContext: NodeContext[N, E] = contexts.get(edge.from) match {
      case Some(fromContext) => fromContext.copy(outgoing = fromContext.outgoing :+ edge)
      case None => NodeContext(edge.from, outgoing = List(edge))
    }

    val newToContext: NodeContext[N, E] = contexts.get(edge.to) match {
      case Some(toContext) => toContext.copy(incoming = toContext.incoming :+ edge)
      case None => NodeContext(edge.to, incoming = List(edge))
    }

    copy(
      contexts = contexts + (edge.from -> newFromContext) + (edge.to -> newToContext),
      edges = edges + edge
    )
  }

  override def withNode(node: N): Graph[E, N, M] =
    if (contexts.contains(node)) {
      this
    } else copy(contexts = contexts + (node -> NodeContext(node)))

  override def withMeta[MT](meta: MT): Graph[E, N, MT] = copy(meta = meta)
}

/**
 * A representation of a Graph.
 *
 * A graph represents a relationship between objects (called nodes).
 * The relation between two objects is established by an "arrow" (called edge) between
 * two nodes. This arrow has a direction, thereby creating incoming (pointing to a node)
 * and outgoing edges for a node.
 *
 * A graph is parametrized by:
 *
 * @tparam E the value type of the edges
 *
 */
// #graph
trait Graph[E, N, M] {
  def meta: M
  def contexts: scala.collection.Map[N, NodeContext[N, E]]
  def edges: scala.collection.Set[Edge[E, N]]
  def nodes: Iterable[N] = contexts.keys

  def findNode(id: N): Option[NodeContext[N, E]] = contexts.get(id)

  def incoming(node: N): Iterable[Edge[E, N]] = contexts.get(node) match {
    case Some(node) => node.incoming
    case None => Iterable.empty
  }

  def outgoing(node: N): Iterable[Edge[E, N]] = contexts.get(node) match {
    case Some(node) => node.outgoing
    case None => Iterable.empty
  }

  def successors(node: N): Iterable[N] = outgoing(node).map(_.to)
  def predecessors(node: N): Iterable[N] = incoming(node).map(_.from)

  def +(edge: Edge[E, N]): Graph[E, N, M]
  def withNode(node: N): Graph[E, N, M]

  def withNodes(nodes: Iterable[N]): Graph[E, N, M] = nodes.foldLeft(this)(_ withNode _)
  def withEdges(edges: Iterable[Edge[E, N]]): Graph[E, N, M] = edges.foldLeft(this)(_ + _)
  def withMeta[MT](meta: MT): Graph[E, N, MT]
}

// #graph

object Graph {
  def apply[E, N, M](meta: M,
                     edges: Iterable[Edge[E, N]] = Iterable.empty,
                     nodes: Iterable[N] = Iterable.empty): Graph[E, N, M] =
    GraphInstance(meta).withEdges(edges).withNodes(nodes)

  def empty[E, N, M](meta: M): Graph[E, N, M] = GraphInstance[E, N, M](meta)

  def unit[E, N]: Graph[E, N, Unit] = GraphInstance[E, N, Unit](())

  /**
   * utility method to create a unit typed graph quickly from iterable edges
   *
   * @param edges the edges to create the graph from
   * @tparam E the edge type
   * @tparam N the node type
   * @return a typed graph with the edges
   */
  def fromEdges[E, N](edges: Iterable[Edge[E, N]]): Graph[E, N, Unit] =
    edges.foldLeft(unit[E, N])(_ + _)
}
