package com.flowtick.graphs

/**
 * Type class to define an identifier for a value
 *
 * Similar to the Show type class but mainly used for serialisation purposes
 *
 * @tparam T the node type
 */
// #identifiable
trait Identifiable[-T] {
  def apply(value: T): String
}

object Identifiable {
  /** creates an instance of [[Identifiable]] using the provided function */
  def identify[T](f: T => String): Identifiable[T] = (a: T) => f(a)
}
// #identifiable

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

final case class Edge[+E, +N](id: String, value: E, from: Node[N], to: Node[N]) {
  def map[B](f: E => B): Edge[B, N] = copy(value = f(value))

  def mapNode[B](fromFun: N => B, toFun: N => B): Edge[E, B] = copy(
    from = from.copy(value = fromFun(from.value)),
    to = to.copy(value = toFun(to.value))
  )

  override def toString: String = s"(${from.id}) --($value)--> (${to.id})"
}

object Edge {
  def of[E, N](value: E, from: Node[N], to: Node[N]): Edge[E, N] = Edge(s"${from.id}-${to.id}", value, from, to)
  def unit[N](from: String, to: String): Edge[Unit, String] = Edge.of((), Node(from, from), Node(to, to))
}

final case class Node[+N](id: String, value: N) {
  def map[B](f: N => B): Node[B] = copy(value = f(value))

  override def toString: String = s"Node(id: $id, value: $value)"
}

object Node {
  def of[N](value: N)(implicit id: Identifiable[N]): Node[N] = Node(id(value), value)
}

final case class GraphInstance[E, N](nodesById: scala.collection.Map[String, Node[N]] = scala.collection.immutable.TreeMap.empty[String, Node[N]],
                                     incomingById: scala.collection.Map[String, Set[String]] = scala.collection.immutable.Map.empty[String, Set[String]],
                                     outgoingById: scala.collection.Map[String, Set[String]] = scala.collection.immutable.Map.empty[String, Set[String]],
                                     edgesById: scala.collection.Map[String, Edge[E, N]] = scala.collection.immutable.TreeMap.empty[String, Edge[E, N]]) extends Graph[E, N] {
  def withEdge(edge: Edge[E, N]): Graph[E, N] = {
    copy(
      incomingById = incomingById + (edge.to.id -> (incomingById.getOrElse(edge.to.id, Set.empty) + edge.id)),
      outgoingById = outgoingById + (edge.from.id -> (outgoingById.getOrElse(edge.from.id, Set.empty) + edge.id)),
      nodesById = nodesById + (edge.from.id -> edge.from) + (edge.to.id -> edge.to),
      edgesById = edgesById + (edge.id -> edge)
    )
  }

  def mapNodes[B](f: N => B): Graph[E, B] = copy(
    nodesById = nodesById.mapValues(_.map(f)).toMap,
    edgesById = edgesById.mapValues(_.mapNode(f, f)).toMap
  )

  def incoming(nodeId: String): Iterable[Edge[E, N]] = incomingById.getOrElse(nodeId, Iterable.empty).map(edgesById)
  def outgoing(nodeId: String): Iterable[Edge[E, N]] = outgoingById.getOrElse(nodeId, Iterable.empty).map(edgesById)

  override def withNode(node: Node[N]): Graph[E, N] =
    if (nodesById.contains(node.id)) {
      this
    } else copy(
      nodesById = nodesById + (node.id -> node)
    )

  override def nodes: Iterable[Node[N]] = nodesById.values

  override def edges: Iterable[Edge[E, N]] = edgesById.values

  override def findNode(id: String): Option[Node[N]] = nodesById.get(id)
  override def findEdge(id: String): Option[Edge[E, N]] = edgesById.get(id)

  override def nodeIds: Iterable[String] = nodesById.keys

  override def edgeIds: Iterable[String] = edgesById.keys

  override def updateNode(id: String)(f: N => N): Graph[E, N] = nodesById.get(id) match {
    case Some(node) => copy(
      nodesById = nodesById + (node.id -> node),
      edgesById = {
        val relatedEdges = (
          incoming(node.id).map(_.mapNode(identity, f)).iterator ++
          outgoing(node.id).map(_.mapNode(f, identity)).iterator
        ).map(edge => edge.id -> edge).toMap

        edgesById ++ relatedEdges
      }
    )
    case None => this
  }

  override def updateEdge(id: String)(f: E => E): Graph[E, N] = edgesById.get(id) match {
    case Some(edge) => copy(edgesById = edgesById + (edge.id -> edge.map(f)))
    case None => this
  }

  override def toString: String = {
    val nodeString = nodes.mkString(",\n")
    val edgeString = edges.mkString(",\n")
    s"nodes = [\n$nodeString\n],\nedges = [\n$edgeString\n]), incoming: $incomingById, outgoing: $outgoingById"
  }

  override def removeNodeById(nodeId: String): Graph[E, N] = nodesById.get(nodeId) match {
    case Some(node) =>
      val withoutNode: Graph[E, N] = copy(
        nodesById = nodesById - node.id,
        incomingById = incomingById - node.id,
        outgoingById = outgoingById - node.id,
      )

      (outgoingById.getOrElse(node.id, Set.empty) ++ incomingById.getOrElse(node.id, Set.empty))
        .foldLeft(withoutNode)(_ removeEdgeById _)
    case None => this
  }

  override def removeEdgeById(edgeId: String): Graph[E, N] = edgesById.get(edgeId) match {
    case Some(edge) =>
      val newIncoming = incomingById.getOrElse(edge.to.id, Set.empty) - edge.id
      val newOutgoing = outgoingById.getOrElse(edge.from.id, Set.empty) - edge.id

      copy(
        edgesById = edgesById - edge.id,
        incomingById = if (newIncoming.nonEmpty) incomingById + (edge.to.id -> newIncoming) else incomingById - edge.to.id,
        outgoingById = if (newOutgoing.nonEmpty) outgoingById + (edge.from.id -> newOutgoing) else outgoingById - edge.from.id,
      )
    case None => this
  }
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
trait Graph[E, N] {
  def edges: Iterable[Edge[E, N]]
  def nodes: Iterable[Node[N]]

  def nodeIds: Iterable[String]
  def edgeIds: Iterable[String]

  def findNode(id: String): Option[Node[N]]
  def findEdge(id: String): Option[Edge[E, N]]

  def updateNode(id: String)(f: N => N): Graph[E, N]
  def updateEdge(id: String)(f: E => E): Graph[E, N]

  def removeNodeById(nodeId: String): Graph[E, N]
  def removeEdgeById(edgeId: String): Graph[E, N]

  def removeNodeValue(node: N)(implicit nodeId: Identifiable[N]): Graph[E, N] = removeNodeById(nodeId(node))
  def removeEdge(edge: Edge[E, N]): Graph[E, N] = removeEdgeById(edge.id)

  def incoming(nodeId: String): Iterable[Edge[E, N]]
  def outgoing(nodeId: String): Iterable[Edge[E, N]]

  def successors(nodeId: String): Iterable[Node[N]] = outgoing(nodeId).flatMap(edge => findNode(edge.to.id))
  def predecessors(nodeId: String): Iterable[Node[N]] = incoming(nodeId).flatMap(edge => findNode(edge.from.id))

  def mapNodes[B](f: N => B): Graph[E, B]

  def withEdge(edge: Edge[E, N]): Graph[E, N]
  def withNode(node: Node[N]): Graph[E, N]

  def addEdge(value: E, from: N, to: N)(implicit nodeId: Identifiable[N]): Graph[E, N] =
    withEdge(Edge.of(value, Node(nodeId(from), from), Node(nodeId(to), to)))

  def addNode(value: N)(implicit nodeId: Identifiable[N]): Graph[E, N] =
    withNode(Node(nodeId(value), value))

  def +(edge: Edge[E, N]): Graph[E, N] = withEdge(edge)

  def withNodes(nodes: Iterable[Node[N]]): Graph[E, N] = nodes.foldLeft(this)(_ withNode _)
  def withEdges(edges: Iterable[Edge[E, N]]): Graph[E, N] = edges.foldLeft(this)(_ withEdge _)
}

// #graph

object Graph {

  def apply[E, N](edges: Iterable[Edge[E, N]] = Iterable.empty,
                  nodes: Iterable[Node[N]] = Iterable.empty): Graph[E, N] =
    empty.withEdges(edges).withNodes(nodes)

  def empty[E, N]: Graph[E, N] = GraphInstance[E, N]()

  /**
   * utility method to create a unit typed graph quickly from iterable edges
   *
   * @param edges the edges to create the graph from
   * @tparam E the edge type
   * @tparam N the node type
   * @return a typed graph with the edges
   */
  def fromEdges[E, N](edges: Iterable[Edge[E, N]]): Graph[E, N] =
    edges.foldLeft(empty[E, N]) {
      (acc, edge) => acc
        .withNode(edge.from)
        .withNode(edge.to) withEdge edge
    }

  def fromNodes[E, N](nodes: Map[String, Node[N]]): Graph[E, N] =
    GraphInstance(nodesById = nodes)
}
