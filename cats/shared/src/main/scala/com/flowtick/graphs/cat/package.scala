package com.flowtick.graphs

import cats.{Applicative, Eval, Monoid, Traverse}

package object cat {

  /** A monoid to combine graphs.
    * @param nodeId
    *   the id for the new nodes
    * @tparam E
    *   edge type
    * @tparam N
    *   node type
    */
  class GraphMonoid[E, N](nodeId: Identifiable[N], edgeId: Identifiable[E])
      extends Monoid[Graph[E, N]] {
    override def empty: Graph[E, N] = Graph.empty[E, N](nodeId, edgeId)

    override def combine(x: Graph[E, N], y: Graph[E, N]): Graph[E, N] =
      (x.edges ++ y.edges)
        .foldLeft(Graph.empty[E, N](nodeId, edgeId))(_ withEdge _)
        .withNodes(x.nodes ++ y.nodes)
  }

  object GraphMonoid {
    def apply[E, N](implicit nodeId: Identifiable[N], edgeId: Identifiable[E]) =
      new GraphMonoid[E, N](nodeId, edgeId)
  }

  class GraphNodeApplicative[E](implicit id: Identifiable[Any]) extends Applicative[Graph[E, *]] {
    override def pure[A](x: A): Graph[E, A] = Graph(nodes = Set(Node.of(x)(id)))

    override def ap[A, B](functionGraph: Graph[E, A => B])(graph: Graph[E, A]): Graph[E, B] =
      GraphMonoid[E, B].combineAll(graph.nodes.map(node => {
        functionGraph.nodes.foldLeft(Graph.empty[E, B]) { case (acc, f) =>
          graph.outgoing(node.id).foldLeft(acc) { case (result, edge) =>
            val fromNode = f.value(node.value)
            graph
              .findNode(edge.to)
              .map(existingToNode => {
                val toNode = f.value(existingToNode.value)

                result
                  .addNode(fromNode)
                  .addNode(toNode)
                  .withEdgeValue(edge.value, id(fromNode), id(toNode))
              })
              .getOrElse(result)
          }
        }
      }))
  }

  object GraphApplicative {
    def apply[E](implicit id: Identifiable[Any]): Applicative[Graph[E, *]] =
      new GraphNodeApplicative[E]
  }

  class GraphNodeTraverse[E](implicit id: Identifiable[Any]) extends Traverse[Graph[E, *]] {
    private implicit val ga: Applicative[Graph[E, *]] = GraphApplicative[E]
    import cats.implicits._

    override def traverse[G[_], A, B](
        fa: Graph[E, A]
    )(f: A => G[B])(implicit ap: Applicative[G]): G[Graph[E, B]] = {
      val applied: Graph[E, G[(String, B)]] = fa.map(a => {
        f(a).map(b => (fa.nodeId(a), b))
      })

      Traverse[List].sequence(applied.nodes.map(_.value).toList).map { nodeListWithId =>
        {
          val nodeMap = nodeListWithId.toMap
          Graph
            .empty[E, B]
            .addNodes(nodeMap.values)
            .withEdges(fa.edges.flatMap(edge => {
              for {
                fromValue <- nodeMap.get(edge.from)
                toValue <- nodeMap.get(edge.to)
              } yield Edge.of(edge.value, id(fromValue), id(toValue))
            }))
        }
      }
    }

    override def foldLeft[A, B](fa: Graph[E, A], b: B)(f: (B, A) => B): B =
      Traverse[List].foldLeft(fa.nodes.map(_.value).toList, b)(f)
    override def foldRight[A, B](fa: Graph[E, A], lb: Eval[B])(
        f: (A, Eval[B]) => Eval[B]
    ): Eval[B] =
      Traverse[List].foldRight(fa.nodes.map(_.value).toList, lb)(f)
  }

  object GraphNodeTraverse {
    def apply[E](implicit id: Identifiable[Any]): Traverse[Graph[E, *]] = new GraphNodeTraverse[E]
  }

  trait GraphInstances {
    implicit def graphMonoid[E, N](implicit
        nodeId: Identifiable[N],
        edgeId: Identifiable[E]
    ): Monoid[Graph[E, N]] =
      GraphMonoid[E, N]
    implicit def graphNodeApplicative[E](implicit id: Identifiable[Any]): Applicative[Graph[E, *]] =
      GraphApplicative[E]
  }

  object instances extends GraphInstances
}
