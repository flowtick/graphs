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
  def identify[A](f: A => String): Identifiable[A] = (a: A) => f(a)
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

final case class Node[X](value: X, label: Option[String] = None) {
  override def toString: String = s"${value.toString}${label.map(" (" + _ + ")").getOrElse("")}"
}

final case class Edge[V, N](value: V, head: N, tail: N) {
  override def toString: String = {
    val valueString = if (value != ()) s"[${value.toString}]" else ""

    s"${head.toString} --> ${tail.toString}$valueString"
  }
}

/**
 * The context of a node in a graph
 *
 * @param incoming the incoming edges (pointing to that node)
 * @param outgoing the outgoing edges (pointing to another node)
 * @tparam V the value type of the edges
 * @tparam N the value type of the nodes
 */
final case class NodeContext[V, N](
  incoming: Set[Edge[V, N]],
  outgoing: Set[Edge[V, N]]) {
  def map[B, C](nodeFn: N => B, edgeFn: Edge[V, N] => C): NodeContext[C, B] = {
    NodeContext(
      incoming.map(edge => Edge(edgeFn(edge), nodeFn(edge.head), nodeFn(edge.tail))),
      outgoing.map(edge => Edge(edgeFn(edge), nodeFn(edge.head), nodeFn(edge.tail))))
  }
}

object NodeContext {
  private val EmptyNodeContext = NodeContext[Any, Any](Set.empty, Set.empty)

  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def empty[V, N]: NodeContext[V, N] = EmptyNodeContext.asInstanceOf[NodeContext[V, N]]
}

/**
 * A representation of a Graph.
 *
 * A graph represents a relationship between objects (called nodes).
 * The relation between two objects is established by an "arrow" (call edge) between
 * two nodes. This arrow has a direction, thereby creating incoming (pointing to a node)
 * and outgoing edges for a node.
 *
 * A graph is parametrized by:
 *
 * @tparam V the value type of the edges
 * @tparam N the value type of the nodes
 * @tparam M the value type of the graph
 *
 */
// #graph
trait Graph[V, N, M] {
  def value: M

  def nodeContext: scala.collection.Map[N, NodeContext[V, N]]

  def nodes: Iterable[N]

  def edges: Iterable[Edge[V, N]]

  def outgoing(node: N): Set[Edge[V, N]]

  def incoming(node: N): Set[Edge[V, N]]

  def predecessors(node: N): Iterator[N]

  def successors(node: N): Iterator[N]
}

abstract class GraphBase[V, N, M] extends Graph[V, N, M] {
  private lazy val lazyNodes: Iterable[N] = nodeContext.keys

  /**
   * TODO (maybe) this could be optimized, in a graph were everything points to one node
   * this would iterate still everything
   */
  private lazy val lazyEdges: Iterable[Edge[V, N]] = nodeContext.iterator.flatMap {
    case (_, context) => context.incoming.iterator
  }.toIterable

  def nodes: Iterable[N] = lazyNodes

  def edges: Iterable[Edge[V, N]] = lazyEdges

  def outgoing(node: N): Set[Edge[V, N]] = nodeContext.get(node).map(_.outgoing).getOrElse(Set.empty)

  def incoming(node: N): Set[Edge[V, N]] = nodeContext.get(node).map(_.incoming).getOrElse(Set.empty)

  def predecessors(node: N): Iterator[N] = incoming(node).iterator.map(_.head)

  def successors(node: N): Iterator[N] = outgoing(node).iterator.map(_.tail)
}
// #graph

/**
 *
 * @param value the value of the graph (of type M)
 * @param nodeContext a mapping of nodes to their context (the incoming and outgoing edges)
 */
final case class ImmutableGraph[V, N, M](
  value: M,
  nodeContext: scala.collection.Map[N, NodeContext[V, N]]) extends GraphBase[V, N, M]

final case class MutableGraph[V, N, M](
  value: M,
  nodeContext: scala.collection.mutable.Map[N, NodeContext[V, N]]) extends GraphBase[V, N, M]

object Graph {
  def empty[V, N, M](meta: M): Graph[V, N, M] = ImmutableGraph(meta, nodeContext = Map.empty)

  def from[V, N, M](
    value: M,
    nodes: Iterable[N] = Iterable.empty,
    edges: Iterable[Edge[V, N]]): Graph[V, N, M] =
    edges.foldLeft {
      nodes.foldLeft(GraphBuilder[V, N, M](value))(_ withNode _)
    }(_ withEdge _).build

  /**
   * utility method to create a unit typed graph quickly from iterable edges
   *
   * @param edges the edges to create the graph from
   * @tparam V
   * @tparam N
   * @return a unit typed graph with the edges
   */
  def fromEdges[V, N](edges: Iterable[Edge[V, N]]): Graph[V, N, Unit] = from(value = (), edges = edges)
}

/**
 * A mutable builder of graph instances.
 *
 * @param nodeContext internal mutable map of the node contexts
 * @param value the graph value
 * @tparam V the value type of the edges
 * @tparam N the value type of the nodes
 * @tparam M the value type of the graph
 */
final case class MutableGraphBuilder[V, N, M](
  private val nodeContext: scala.collection.mutable.HashMap[N, NodeContext[V, N]],
  private val value: M) {
  private def building(block: => Any): MutableGraphBuilder[V, N, M] = { block; this }

  def withEdge(edge: Edge[V, N]): MutableGraphBuilder[V, N, M] = building {
    val headContext = nodeContext.getOrElse(edge.head, NodeContext.empty[V, N])
    nodeContext.put(edge.head, headContext.copy(outgoing = headContext.outgoing + edge))

    val tailContext = nodeContext.getOrElse(edge.tail, NodeContext.empty[V, N])
    nodeContext.put(edge.tail, tailContext.copy(incoming = tailContext.incoming + edge))
  }

  def withNode(node: N): MutableGraphBuilder[V, N, M] = building {
    nodeContext.put(node, NodeContext.empty[V, N])
  }

  def mutable: Graph[V, N, M] = MutableGraph(value, nodeContext)

  def build: Graph[V, N, M] = ImmutableGraph(value, nodeContext.toMap)
}

object GraphBuilder {
  def fromGraph[V, N, M](graph: Graph[V, N, M]): MutableGraphBuilder[V, N, M] = {
    val mutableContext = graph.nodeContext.foldLeft(scala.collection.mutable.HashMap[N, NodeContext[V, N]]()) {
      case (mutableMap, (node, context)) =>
        mutableMap.put(node, context)
        mutableMap
    }

    MutableGraphBuilder(mutableContext, graph.value)
  }
  def apply[V, N, M](value: M): MutableGraphBuilder[V, N, M] = MutableGraphBuilder(scala.collection.mutable.HashMap[N, NodeContext[V, N]](), value)
  def unit[V, N]: MutableGraphBuilder[V, N, Unit] = apply(value = ())
}
