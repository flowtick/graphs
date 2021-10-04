package com.flowtick.graphs

import cats.{Applicative, Monoid}

package object cat {

  /** A monoid to combine graphs.
    * @param nodeId
    *   the id for the new nodes
    * @tparam E
    *   edge type
    * @tparam N
    *   node type
    */
  class GraphMonoid[E, N](nodeId: Identifiable[N]) extends Monoid[Graph[E, N]] {
    override def empty: Graph[E, N] = Graph.empty[E, N](nodeId)

    override def combine(x: Graph[E, N], y: Graph[E, N]): Graph[E, N] =
      (x.edges ++ y.edges)
        .foldLeft(Graph.empty[E, N](nodeId))(_ withEdge _)
        .withNodes(x.nodes ++ y.nodes)
  }

  object GraphMonoid {
    def apply[E, N](implicit nodeId: Identifiable[N]) = new GraphMonoid[E, N](nodeId)
  }

  class GraphNodeApplicative[E](implicit id: Identifiable[Any])
      extends Applicative[({ type GraphType[T] = Graph[E, T] })#GraphType] {
    override def pure[A](x: A): Graph[E, A] = Graph(nodes = Set(Node.of(x)(id)))

    override def ap[A, B](functionGraph: Graph[E, A => B])(graph: Graph[E, A]): Graph[E, B] =
      GraphMonoid[E, B].combineAll(graph.nodes.map(node => {
        Graph
          .empty[E, B]
          .addNodes(
            functionGraph.nodes
              .filter(node => graph.outgoing(node.id).isEmpty && graph.incoming(node.id).isEmpty)
              .map(_.value(node.value))
          )
          .withEdges(
            functionGraph.nodes.flatMap(f =>
              graph
                .outgoing(node.id)
                .flatMap(edge => {
                  for {
                    toNode <- graph.findNode(edge.to)
                  } yield Edge.of(edge.value, id(f.value(node.value)), id(f.value(toNode.value)))
                })
            )
          )
      }))
  }

  object GraphApplicative {
    def apply[E](implicit id: Identifiable[Any]) = new GraphNodeApplicative[E]
  }

  trait GraphInstances {
    implicit def graphMonoid[E, N](implicit nodeId: Identifiable[N]): Monoid[Graph[E, N]] =
      GraphMonoid[E, N]
    implicit def graphNodeApplicative[E](implicit id: Identifiable[Any]): GraphNodeApplicative[E] =
      GraphApplicative[E]
  }

  object instances extends GraphInstances
}
