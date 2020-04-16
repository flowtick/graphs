package com.flowtick.graphs

import cats.Monoid

package object cat {

  class GraphMonoid[E, N, M](implicit metaMonoid: Monoid[M]) extends Monoid[Graph[E, N, M]] {
    override def empty: Graph[E, N, M] = Graph.empty(metaMonoid.empty)

    override def combine(x: Graph[E, N, M], y: Graph[E, N, M]): Graph[E, N, M] = GraphInstance(
      meta = metaMonoid.combine(x.meta, y.meta),
      contexts = x.contexts ++ y.contexts
    ).withEdges(x.edges ++ y.edges)
  }

  trait GraphInstances {
    implicit def graphMonoid[E, N, M](implicit metaMonoid: Monoid[M]): Monoid[Graph[E, N, M]] = new GraphMonoid[E, N, M]
  }

  object instances extends GraphInstances
}
