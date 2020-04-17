package com.flowtick.graphs

import cats.{Applicative, Monoid}

package object cat {

  /**
   * A monoid to combine graphs.
   *
   * @param metaMonoid a monoid to combine the meta values
   * @tparam M
   * @tparam E
   * @tparam N
   */
  class GraphMonoid[M, E, N](implicit metaMonoid: Monoid[M]) extends Monoid[Graph[M, E, N]] {
    override def empty: Graph[M, E, N] = Graph.empty(metaMonoid.empty)

    override def combine(x: Graph[M, E, N], y: Graph[M, E, N]): Graph[M, E, N] =
      (x.edges ++ y.edges)
        .foldLeft(Graph.empty[M, E, N](metaMonoid.combine(x.meta, y.meta)))(_ + _)
        .withNodes(x.nodes ++ y.nodes)
  }

  object GraphMonoid {
    def apply[M, E, N](implicit metaMonoid: Monoid[M]) = new GraphMonoid[M, E, N]
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
   * @param metaMonoid a monoid to combine the meta values
   * @tparam M meta type
   * @tparam E edge type
   */
  class GraphNodeApplicative[M, E](implicit metaMonoid: Monoid[M]) extends Applicative[({ type GraphType[T] = Graph[M, E, T] })#GraphType] {
    override def pure[A](x: A): Graph[M, E, A] = Graph(metaMonoid.empty, nodes = Set(x))

    override def ap[A, B](ff: Graph[M, E, A => B])(fa: Graph[M, E, A]): Graph[M, E, B] =
      GraphMonoid[M, E, B].combineAll(ff.nodes.map(fa.mapNodes))
  }

  object GraphApplicative {
    def apply[M, E](implicit metaMonoid: Monoid[M]) = new GraphNodeApplicative[M, E]
  }

  trait GraphInstances {
    implicit def graphMonoid[M, E, N](implicit metaMonoid: Monoid[M]): Monoid[Graph[M, E, N]] = GraphMonoid[M, E, N]
    implicit def graphNodeApplicative[M, E](implicit metaMonoid: Monoid[M]): GraphNodeApplicative[M, E] = GraphApplicative[M, E]
  }

  object instances extends GraphInstances
}
