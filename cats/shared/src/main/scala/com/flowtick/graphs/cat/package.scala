package com.flowtick.graphs

import cats.{Applicative, Monoid}

package object cat {

  /**
   * A monoid to combine graphs.
   *
   * @tparam E
   * @tparam N
   */
  class GraphMonoid[E, N] extends Monoid[Graph[E, N]] {
    override def empty: Graph[E, N] = Graph.empty

    override def combine(x: Graph[E, N], y: Graph[E, N]): Graph[E, N] =
      (x.edges ++ y.edges)
        .foldLeft(Graph.empty[E, N])(_ withEdge _)
        .withNodes(x.nodes ++ y.nodes)
  }

  object GraphMonoid {
    def apply[E, N] = new GraphMonoid[E, N]
  }

  /**
   * An applicative for the nodes of a graph.
   *
   * In the current design the edge identity includes the value of the edge and the nodes itself.
   * So if we have a function that maps the nodes, we can create a new graph with the mapped nodes and map also
   * the nodes of the edges while keeping the its value (the mapping itself is delegated to the graph instance).
   *
   * We can't provide a Monad instance (for nodes) here because the [[cats.Monad.flatMap]] signature gives only
   * a function that maps a node A to a graph with nodes B, but that is not enough to flatten / merge
   * it with the original graph edges, which still have node type A.
   *
   * Note: the [[Identifiable]] acts as an id generator in this case, since we need to be able to create new ids
   * for arbitrary types.
   *
   * @tparam E edge type
   */
  class GraphNodeApplicative[E](id: Identifiable[Any]) extends Applicative[({ type GraphType[T] = Graph[E, T] })#GraphType] {
    override def pure[A](x: A): Graph[E, A] = Graph(nodes = Set(Node.of(x)(id)))

    override def ap[A, B](ff: Graph[E, A => B])(fa: Graph[E, A]): Graph[E, B] =
      GraphMonoid[E, B].combineAll(ff.nodes.map(node => fa.mapNodes(node.value)))
  }

  object GraphApplicative {
    def apply[E](implicit id: Identifiable[Any]) = new GraphNodeApplicative[E](id)
  }

  trait GraphInstances {
    implicit def graphMonoid[E, N]: Monoid[Graph[E, N]] = GraphMonoid[E, N]
    implicit def graphNodeApplicative[E](implicit id: Identifiable[Any] = defaults.anyId.identifyAny): GraphNodeApplicative[E] = GraphApplicative[E]
  }

  object instances extends GraphInstances
}
