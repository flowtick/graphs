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

/**
 * Type class to define an weight for a type, in this context, mainly used for edges.
 *
 * @tparam W type of the value to be weighted
 * @tparam V weight value type
 */
trait Weighted[W, V] {
  def value(weighted: W): V
}

/**
 * The base type for edge in a graph, which connects incoming nodes to outgoing nodes of the same type.
 *
 * @tparam E the edge value type
 * @tparam N the base type of the nodes to connect
 */
trait Edge[E, N] {
  def value: E

  def map[B](f: E => B): Edge[B, N] = SomeEdge(f(value), predecessors, successors)

  /**
   * @return the set of predecessors for an edge, this is a set to allow multi graphs and hyper edges.
   *         In the more common directed and undirected edge types, this will be a single value or empty set.
   *         Empty means no predecessors and is useful to model graphs that allow unconnected nodes.
   */
  def predecessors: Set[N]

  /**
   * @return complement of predecessors, see corresponding docs
   */
  def successors: Set[N]

  /**
   * shortcut for single element predecessors
   */
  def left: Option[N] = predecessors.headOption

  /**
   * shortcut for single element successors
   */
  def right: Option[N] = successors.headOption

  def incoming(node: N, graph: Graph[N, E]): Iterable[Edge[E, N]] = graph.edges.flatMap(edge => if (edge.successors.contains(node)) Some(edge) else None)
  def outgoing(node: N, graph: Graph[N, E]): Iterable[Edge[E, N]] = graph.edges.flatMap(edge => if (edge.predecessors.contains(node)) Some(edge) else None)
}

final case class SomeEdge[E, N](value: E, predecessors: Set[N], successors: Set[N]) extends Edge[E, N]

/**
 * an edge builder allows to create an edge from another type. typically this will be some kind of tuple type
 * like (N, N), but this might vary depending on the edge type. See weighted edges in the defaults package for reference.
 *
 * @tparam N node tyoe
 * @tparam E edge type
 * @tparam B the builder type
 */
trait EdgeBuilder[N, E, B] {
  def create(from: B)(implicit identifiable: Identifiable[N]): Edge[E, N]
}

/**
 * Wikipedia (https://en.wikipedia.org/wiki/Graph_(discrete_mathematics)):
 * "a graph is a structure amounting to a set of objects in which some pairs of the objects are in some sense "related"."
 *
 * In this case the objects are called nodes and the relation is expressed as a set [[Edge]]s
 * between nodes
 *
 * @tparam N the node type
 * @tparam E the edge type
 */
// #graph
trait Graph[N, E] {
  def nodes: Set[N]
  def edges: Set[Edge[E, N]]

  def incoming(node: N): Iterable[Edge[E, N]] = edges.flatMap(_.incoming(node, this))
  def outgoing(node: N): Iterable[Edge[E, N]] = edges.flatMap(_.outgoing(node, this))
}

final case class SomeGraph[N, E](nodes: Set[N], edges: Set[Edge[E, N]]) extends Graph[N, E]

object Graph {
  def apply[N, E](nodes: Set[N], edges: Set[Edge[E, N]]): Graph[N, E] = SomeGraph(nodes, edges)

  def empty[N, E]: Graph[N, E] = EmptyGraph[N, E]()

  final case class EmptyGraph[N, E]() extends Graph[N, E] {
    override def nodes: Set[N] = Set.empty
    override def edges: Set[Edge[E, N]] = Set.empty

    override def incoming(node: N): Iterable[Edge[E, N]] = Iterable.empty
    override def outgoing(node: N): Iterable[Edge[E, N]] = Iterable.empty
  }

  def edgeNodes[E, N](edges: Set[Edge[E, N]]): Set[N] = edges.flatMap(edge => edge.predecessors ++ edge.successors)

  def create[N, E, B](tuples: B*)(implicit
    identifiable: Identifiable[N],
    edgeBuilder: EdgeBuilder[N, E, B]): Graph[N, E] = {
    val edges: Set[Edge[E, N]] = tuples.map(edgeBuilder.create).toSet
    apply(edgeNodes(edges), edges)
  }
}
// #graph

