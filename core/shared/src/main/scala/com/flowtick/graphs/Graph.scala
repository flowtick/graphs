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

final case class Node[X](value: X, label: Option[String] = None) {
  override def toString: String = s"${value.toString}${label.map(" (" + _ + ")").getOrElse("")}"
}

final case class Edge[V, N](value: V, head: N, tail: N) {
  override def toString: String = {
    val valueString = if (value != Unit) s"[$value]" else ""

    s"$head --> $tail$valueString"
  }
}

final case class NodeContext[V, N](
  incoming: List[Edge[V, N]],
  outgoing: List[Edge[V, N]])

object NodeContext {
  def empty[V, N]: NodeContext[V, N] = NodeContext[V, N](List.empty, List.empty)
}

// #graph

final case class Graph[V, N, M](
  value: M,
  edges: Iterable[Edge[V, N]],
  nodeContext: scala.collection.Map[N, NodeContext[V, N]]) {
  lazy val nodes: Iterable[N] = nodeContext.keys

  def outgoing(node: N): List[Edge[V, N]] = nodeContext.get(node).map(_.outgoing).getOrElse(List.empty)

  def incoming(node: N): List[Edge[V, N]] = nodeContext.get(node).map(_.incoming).getOrElse(List.empty)

  def predecessors(node: N): Iterable[N] =
    nodeContext.getOrElse(node, NodeContext.empty[V, N]).incoming.view.map(_.head)

  def successors(node: N): Iterable[N] =
    nodeContext.getOrElse(node, NodeContext.empty[V, N]).outgoing.view.map(_.tail)
}

object Graph {
  def empty[V, N, M](meta: M): Graph[V, N, M] = Graph[V, N, M](meta, Iterable.empty, Map.empty)

  def of[V, N, M](value: M, nodes: Option[Iterable[N]] = None)(edges: Iterable[Edge[V, N]])(implicit edgeOrdering: Ordering[Edge[V, N]]): Graph[V, N, M] =
    create(value, nodes.getOrElse(Iterable.empty), edges)

  def from[V, N, M](edges: Iterable[Edge[V, N]], nodes: Option[Iterable[N]] = None)(implicit edgeOrdering: Ordering[Edge[V, N]]): Graph[V, N, Unit] =
    create((), nodes.getOrElse(Iterable.empty), edges)

  def create[V, N, M](
    value: M,
    nodes: Iterable[N],
    edges: Iterable[Edge[V, N]])(implicit edgeOrdering: Ordering[Edge[V, N]]): Graph[V, N, M] = {
    import scala.collection.mutable

    val nodeContext = mutable.HashMap[N, NodeContext[V, N]]()

    nodes.foreach { node =>
      nodeContext.put(node, NodeContext.empty[V, N])
    }

    edges.foreach { edge =>
      val head = edge.head
      val headContext = nodeContext.getOrElse(head, NodeContext.empty[V, N])

      nodeContext.put(head, headContext.copy(outgoing = headContext.outgoing :+ edge))

      val tail = edge.tail
      val tailContext = nodeContext.getOrElse(tail, NodeContext.empty[V, N])

      nodeContext.put(tail, tailContext.copy(incoming = tailContext.incoming :+ edge))
    }

    Graph(value, edges, nodeContext)
  }
}

// #graph