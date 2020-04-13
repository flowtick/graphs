package com.flowtick.graphs

import cats.Monoid

package object cat {

  class GraphMonoid[E, N] extends Monoid[Graph[E, N]] {
    override def empty: Graph[E, N] = Graph.empty

    override def combine(x: Graph[E, N], y: Graph[E, N]): Graph[E, N] = GraphInstance(
      contexts = x.contexts ++ y.contexts,
    ).withEdges(x.edges ++ y.edges)
  }

  trait GraphInstances {
    implicit def graphMonoid[E, N] = new GraphMonoid[E, N]
  }

  object instances extends GraphInstances
}
