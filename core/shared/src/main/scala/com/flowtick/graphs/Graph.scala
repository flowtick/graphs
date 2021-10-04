package com.flowtick.graphs

/** Type class to define an identifier for a value
  *
  * Similar to the Show type class but mainly used for serialisation purposes
  *
  * @tparam T
  *   the node type
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

/** Type class to define an edge label
  *
  * @tparam E
  *   edge value type
  * @tparam L
  *   label type
  */
trait Labeled[E, L] {
  def apply(edge: E): L
}

object Labeled {

  /** creates an instance of [[Labeled]] using the provided function */
  def label[T, I](f: T => I): Labeled[T, I] = (a: T) => f(a)

  def identity[T]: Labeled[T, T] = label => label
}

/** An edge value, note that this only holds the ids of the nodes. You need to get the node values
  * from the graph. This allows dangling references and removes the dependency to the node values at
  * the creation time of the edge. That design comes from a focus on serialization but also makes
  * operations like mapping nodes easier internally (as you do not need to rebuild the edges etc.).
  *
  * @tparam E
  *   the edge type
  */
final case class Edge[+E](id: String, value: E, from: String, to: String) {
  def map[B](f: E => B): Edge[B] = copy(value = f(value))

  override def toString: String = s"$id: (${from}) --($value)--> (${to})"
}

/** Edge with complete node values, makes creating edges more type safe as it can capture both
  * types, which the Edge type can not.
  *
  * Mainly used for `Graph.fromEdges`
  */
final case class Relation[+E, +N](
    value: E,
    from: Node[N],
    to: Node[N],
    symmetric: Boolean = false
) {
  def toEdges: Iterable[Edge[E]] =
    if (symmetric)
      Iterable(Edge.of(value, from.id, to.id), Edge.of(value, to.id, from.id))
    else Iterable(Edge.of(value, from.id, to.id))
}

object Edge {
  def of[E, N](value: E, from: String, to: String): Edge[E] =
    Edge(s"$from-$to", value, from, to)
  def unit[N](from: String, to: String): Edge[Unit] = Edge.of((), from, to)
}

final case class Node[+N](id: String, value: N) {
  def map[B](f: N => B): Node[B] = copy(value = f(value))

  override def toString: String = s"($id: '$value')"
}

object Node {
  def of[N](value: N)(implicit id: Identifiable[N]): Node[N] =
    Node(id(value), value)
}

private[graphs] final case class GraphInstance[E, N](
    nodeId: Identifiable[N],
    nodesById: scala.collection.Map[String, Node[N]] =
      scala.collection.immutable.TreeMap.empty[String, Node[N]],
    incomingById: scala.collection.Map[String, Set[String]] =
      scala.collection.immutable.Map.empty[String, Set[String]],
    outgoingById: scala.collection.Map[String, Set[String]] =
      scala.collection.immutable.Map.empty[String, Set[String]],
    edgesById: scala.collection.Map[String, Edge[E]] =
      scala.collection.immutable.TreeMap.empty[String, Edge[E]]
) extends Graph[E, N] {
  def withEdge(edge: Edge[E]): Graph[E, N] = {
    copy(
      incomingById = incomingById + (edge.to -> (incomingById
        .getOrElse(edge.to, Set.empty) + edge.id)),
      outgoingById = outgoingById + (edge.from -> (outgoingById
        .getOrElse(edge.from, Set.empty) + edge.id)),
      edgesById = edgesById + (edge.id -> edge)
    )
  }

  def incoming(nodeId: String): Iterable[Edge[E]] =
    incomingById.getOrElse(nodeId, Iterable.empty).map(edgesById)
  def outgoing(nodeId: String): Iterable[Edge[E]] =
    outgoingById.getOrElse(nodeId, Iterable.empty).map(edgesById)

  override def withNode(node: Node[N]): Graph[E, N] =
    copy(
      nodesById = nodesById + (node.id -> node)
    )

  override def nodes: Iterable[Node[N]] = nodesById.values
  override def edges: Iterable[Edge[E]] = edgesById.values

  override def findNode(id: String): Option[Node[N]] = nodesById.get(id)
  override def findEdge(id: String): Option[Edge[E]] = edgesById.get(id)

  override def nodeIds: Iterable[String] = nodesById.keys
  override def edgeIds: Iterable[String] = edgesById.keys

  override def updateNode(id: String)(f: N => N): Graph[E, N] =
    nodesById.get(id) match {
      case Some(node) => copy(nodesById = nodesById + (node.id -> node.map(f)))
      case None       => this
    }

  override def updateEdge(id: String)(f: E => E): Graph[E, N] =
    edgesById.get(id) match {
      case Some(edge) => copy(edgesById = edgesById + (edge.id -> edge.map(f)))
      case None       => this
    }

  override def toString: String = {
    val nodeString = nodes.mkString("  ", ",\n  ", "  ")
    val edgeString = edges.mkString("  ", ",\n  ", "  ")
    s"nodes = [\n$nodeString\n],\nedges = [\n$edgeString\n]), incoming: $incomingById, outgoing: $outgoingById"
  }

  override def removeNodeById(nodeId: String): Graph[E, N] =
    nodesById.get(nodeId) match {
      case Some(node) =>
        val withoutNode: Graph[E, N] = copy(
          nodesById = nodesById - node.id,
          incomingById = incomingById - node.id,
          outgoingById = outgoingById - node.id
        )

        (outgoingById.getOrElse(node.id, Set.empty) ++ incomingById.getOrElse(
          node.id,
          Set.empty
        )).foldLeft(withoutNode)(_ removeEdgeById _)
      case None => this
    }

  override def removeEdgeById(edgeId: String): Graph[E, N] =
    edgesById.get(edgeId) match {
      case Some(edge) =>
        val newIncoming = incomingById.getOrElse(edge.to, Set.empty) - edge.id
        val newOutgoing = outgoingById.getOrElse(edge.from, Set.empty) - edge.id

        copy(
          edgesById = edgesById - edge.id,
          incomingById =
            if (newIncoming.nonEmpty) incomingById + (edge.to -> newIncoming)
            else incomingById - edge.to,
          outgoingById =
            if (newOutgoing.nonEmpty) outgoingById + (edge.from -> newOutgoing)
            else outgoingById - edge.from
        )
      case None => this
    }
}

/** A representation of a Graph.
  *
  * A graph represents a relationship between objects (often called nodes). The relation between two
  * objects is established by an "arrow" (called edge) between two nodes. This arrow has a
  * direction, thereby creating incoming (pointing to a node) and outgoing edges for a node.
  *
  * A graph is parametrized by:
  *
  * @tparam E
  *   the value type of the edges
  * @tparam N
  *   the values type of the nodes
  */
// #graph
trait Graph[E, N] {
  def nodeId: Identifiable[N]

  def edges: Iterable[Edge[E]]
  def nodes: Iterable[Node[N]]

  def nodeIds: Iterable[String]
  def edgeIds: Iterable[String]

  def findNode(id: String): Option[Node[N]]
  def findEdge(id: String): Option[Edge[E]]

  def updateNode(id: String)(f: N => N): Graph[E, N]
  def updateEdge(id: String)(f: E => E): Graph[E, N]

  def removeNodeById(nodeId: String): Graph[E, N]
  def removeEdgeById(edgeId: String): Graph[E, N]

  def removeNode(node: Node[N]): Graph[E, N] = removeNodeById(node.id)
  def removeEdge(edge: Edge[E]): Graph[E, N] = removeEdgeById(edge.id)

  def removeNodeValue(node: N): Graph[E, N] = removeNodeById(nodeId(node))

  def incoming(nodeId: String): Iterable[Edge[E]]
  def outgoing(nodeId: String): Iterable[Edge[E]]

  def successors(nodeId: String): Iterable[Node[N]] =
    outgoing(nodeId).flatMap(edge => findNode(edge.to))
  def predecessors(nodeId: String): Iterable[Node[N]] =
    incoming(nodeId).flatMap(edge => findNode(edge.from))

  def addNode(nodeValue: N): Graph[E, N] = withNode(Node(nodeId(nodeValue), nodeValue))
  def addNodes(nodeValues: Iterable[N]): Graph[E, N] = nodeValues.foldLeft(this)(_ addNode _)

  def addEdge(value: E, from: N, to: N): Graph[E, N] =
    withEdge(Edge.of(value, nodeId(from), nodeId(to)))
      .addNode(from)
      .addNode(to)

  def withEdge(edge: Edge[E]): Graph[E, N]
  def withNode(node: Node[N]): Graph[E, N]

  def withEdgeValue(value: E, from: Node[N], to: Node[N]): Graph[E, N] =
    withEdge(Edge.of(value, from.id, to.id)).withNode(from).withNode(to)

  def withNodes(nodes: Iterable[Node[N]]): Graph[E, N] =
    nodes.foldLeft(this)(_ withNode _)
  def withEdges(edges: Iterable[Edge[E]]): Graph[E, N] =
    edges.foldLeft(this)(_ withEdge _)
}

// #graph

object Graph {

  def apply[E, N](
      edges: Iterable[Edge[E]] = Iterable.empty,
      nodes: Iterable[Node[N]] = Iterable.empty
  )(implicit nodeId: Identifiable[N]): Graph[E, N] =
    empty[E, N].withEdges(edges).withNodes(nodes)

  def empty[E, N](implicit nodeId: Identifiable[N]): Graph[E, N] = GraphInstance[E, N](nodeId)

  /** utility method to create a unit typed graph from iterable relations
    *
    * @param relations
    *   the relations to create the graph from
    * @tparam E
    *   the edge type
    * @tparam N
    *   the node type
    * @return
    *   a typed graph with the edges
    */
  def fromEdges[E, N](
      relations: Iterable[Relation[E, N]]
  )(implicit nodeId: Identifiable[N]): Graph[E, N] =
    relations.foldLeft(empty[E, N]) { (acc, relation) =>
      acc
        .withNode(relation.from)
        .withNode(relation.to) withEdges relation.toEdges
    }
}
