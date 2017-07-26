package com.flowtick.graphs

/**
  * The base type for a node in a Graph
  */
trait Node

/**
  * The base type for edge in a graph, which connects two nodes of the same type with each other
  *
  * @tparam N the base type of the nodes to connect
  */
trait Edge[N <: Node] {
  /**
    * optional label to annotate this edge with some human readable text
    *
    * @return
    */
  def label: Option[String]
  def source: N
  def target: N
}

/**
  * The base type for directed edges.
  *
  * As the name indicates a directed edge defines the `direction` in which the source and target are connected, meaning
  * the the graph can only be traversed by going in that direction.
  *
  * For an edge A -> B, that means that B is reachable from A but not vice versa.
  *
  * Obviously, this direction can interpreted depending on the domain that the graph is modeling.
  *
  * @tparam N the base type of the nodes to connect
  */
trait DirectedEdge[N <: Node] extends Edge[N]

/**
  * The base type for undirected edges.
  *
  * An undirected edge defines a connection between two nodes without a particular direction.
  *
  * For an edge A -- B, that means that B is reachable from A and vice versa.
  *
  * An undirected edge can therefore be also imagined as two directed edges, one for each direction.
  *
  * @tparam N the base type of the nodes to connect
  */
trait UndirectedEdge[N <: Node] extends Edge[N]

/**
  * The base type for weighted edges.
  *
  * A weighted edge has a attached numeric value, that can be interpreted depending on the domain
  * e.g. distance / weight / capacity.
  *
  * This numeric value can be used by related algorithms.
  *
  * @tparam N the base type of the nodes to connect
  * @tparam T the type of the weight, needs a Numeric instance
  */
abstract class WeightedEdge[T: Numeric, N <: Node](
  val weight: T,
  val label: Option[String] = None,
  val source: N,
  val target: N
) extends Edge[N]

/**
  * The base type for directed weighted edges.
  */
abstract class WeightedDirectedEdge[T: Numeric, N <: Node](
  override val weight: T,
  override val label: Option[String] = None,
  override val source: N,
  override val target: N
) extends WeightedEdge[T, N](weight, label, source, target)

/**
  * The base type for undirected weighted edges.
  */
abstract class WeightedUndirectedEdge[T: Numeric, N <: Node](
  override val weight: T,
  override val label: Option[String] = None,
  override val source: N,
  override val target: N
) extends WeightedEdge[T, N](weight, label, source, target)

/**
  * Wikipedia (https://en.wikipedia.org/wiki/Graph_(discrete_mathematics)):
  * "a graph is a structure amounting to a set of objects in which some pairs of the objects are in some sense "related"."
  *
  * In this case the objects are called nodes and need to extend the [[Node]]-type and the relation is expressed as [[Edge]]s
  * between this nodes
  *
  * @tparam N the node type
  * @tparam E the edge type
  */
trait Graph[N <: Node, E <: Edge[N]] {
  def nodes: Set[N]
  def edges: Set[E]

  def incoming(node: N): Iterable[E]
  def outgoing(node: N): Iterable[E]
}

/**
  * Base class for creating custom graph types, mainly exists for DRY reasons, so that sub-classes do not need
  * to implement the delegation to the [[GraphBuilder]] on their own.
  *
  * @param graphBuilder for the node and edge types
  * @tparam N the node type
  * @tparam E the edge type
  */
abstract class AbstractGraph[N <: Node, E <: Edge[N]](graphBuilder: GraphBuilder[N, E]) extends Graph[N, E] {
  val edges: Set[E] = graphBuilder.edges.toSet
  val nodes: Set[N] = graphBuilder.nodes.toSet

  def incoming(node: N): Iterable[E] = graphBuilder.incoming.getOrElse(node, Iterable.empty)
  def outgoing(node: N): Iterable[E] = graphBuilder.outgoing.getOrElse(node, Iterable.empty)
}
