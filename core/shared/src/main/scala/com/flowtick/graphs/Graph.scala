package com.flowtick.graphs

/**
 * Type class to define id as string for a node type.
 *
 * Mainly useful for serialisation purposes
 *
 * @tparam T the node type
 */
trait Identifiable[T] {
  def id(node: T): String
}

trait Labeled[E, L] {
  def label(edge: E): Option[L]
}

trait Weighted[W, V] {
  def value(weighted: W): V
}

/**
 * The base type for edge in a graph, which connects two nodes of the same type with each other
 *
 * @tparam N the base type of the nodes to connect
 */
trait Edge[E, N] {
  def first(edge: E): N
  def second(edge: E): Option[N]

  def incoming(node: N, graph: Graph[N, E]): Iterable[E] =
    graph.edges.flatMap(e => if (second(e).contains(node)) Some(e) else None)

  def outgoing(node: N, graph: Graph[N, E]): Iterable[E] =
    graph.edges.flatMap(e => if (first(e) == node) Some(e) else None)
}

trait EdgeBuilder[N, E, B] {
  def create(from: B)(implicit identifiable: Identifiable[N]): E
}

trait GraphBuilder[N, E, G, P] {
  def create(edges: Set[E], nodes: Set[N], graphParams: P): G
}

/**
 * Wikipedia (https://en.wikipedia.org/wiki/Graph_(discrete_mathematics)):
 * "a graph is a structure amounting to a set of objects in which some pairs of the objects are in some sense "related"."
 *
 * In this case the objects are called nodes and the relation is expressed as [[Edge]]s
 * between this nodes
 *
 * @tparam N the node type
 * @tparam E the edge type
 */
// #graph
trait Graph[N, E] {
  def first(edge: E): N
  def second(edge: E): Option[N]

  def nodes: Set[N]
  def edges: Set[E]

  def incoming(node: N): Iterable[E]
  def outgoing(node: N): Iterable[E]
}

abstract class AbstractGraph[N, E](implicit edgeType: Edge[E, N]) extends Graph[N, E] {
  def first(edge: E): N = edgeType.first(edge)
  def second(edge: E): Option[N] = edgeType.second(edge)

  def incoming(node: N): Iterable[E] = edgeType.incoming(node, this)
  def outgoing(node: N): Iterable[E] = edgeType.outgoing(node, this)

  private[graphs] lazy val lazyEdgeNodes: Set[N] = Graph.edgeNodes(edges)

  override def nodes: Set[N] = lazyEdgeNodes
}

object Graph {
  def edgeNodes[E, N](edges: Set[E])(implicit edgeType: Edge[E, N]): Set[N] = edges.flatMap { edge =>
    Set(Some(edgeType.first(edge)), edgeType.second(edge)).flatten
  }

  def create[N, E, G, B, P](tuples: B*)(graphParams: P)(implicit
    identifiable: Identifiable[N],
    edgeType: Edge[E, N],
    edgeBuilder: EdgeBuilder[N, E, B],
    graphBuilder: GraphBuilder[N, E, G, P]): G = {
    val edges: Set[E] = tuples.map(edgeBuilder.create).toSet
    graphBuilder.create(edges, edgeNodes(edges), graphParams)
  }
}
// #graph

