package com.flowtick.graphs

import cats.{Applicative, Monoid}

package object cat {

  class GraphMonoid[M, E, N](implicit metaMonoid: Monoid[M]) extends Monoid[Graph[M, E, N]] {
    override def empty: Graph[M, E, N] = Graph.empty(metaMonoid.empty)

    override def combine(x: Graph[M, E, N], y: Graph[M, E, N]): Graph[M, E, N] =
      (x ++ y).withMeta(metaMonoid.combine(x.meta, y.meta))
  }

  class GraphNodeApplicative[M, E, N](implicit metaMonoid: Monoid[M]) extends Applicative[({ type GraphType[T] = Graph[M, E, T] })#GraphType] {
    override def pure[A](x: A): Graph[M, E, A] = Graph(metaMonoid.empty, nodes = Set(x))

    override def ap[A, B](ff: Graph[M, E, A => B])(fa: Graph[M, E, A]): Graph[M, E, B] = {
      ff.nodes.map { f =>
         fa.contexts.foldLeft(Graph.empty[M, E, B](ff.meta)) {
          case (acc, (node, context)) => acc.withNode(f(node)).withEdges(context.incoming.map(edge => edge.copy(from = f(edge.from), to = f(edge.to))))
        }
      }.foldLeft(Graph.empty[M, E, B](metaMonoid.combine(fa.meta, ff.meta)))(_ ++ _)
    }
  }

  trait GraphInstances {
    implicit def graphMonoid[M, E, N](implicit metaMonoid: Monoid[M]): Monoid[Graph[M, E, N]] = new GraphMonoid[M, E, N]
    implicit def graphNodeApplicative[M, E, N](implicit metaMonoid: Monoid[M]): GraphNodeApplicative[M, E, N] = new GraphNodeApplicative[M, E, N]
  }

  object instances extends GraphInstances
}
