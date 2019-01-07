package com.flowtick.graphs

import cats.{ Functor, Monoid }
import com.flowtick.graphs.Graph.EmptyGraph

package object cat {
  implicit def graphMonoid[N, E]: Monoid[Graph[N, E]] = new Monoid[Graph[N, E]] {
    override def empty: Graph[N, E] = Graph.empty

    override def combine(x: Graph[N, E], y: Graph[N, E]): Graph[N, E] =
      Graph[N, E](x.nodes ++ y.nodes, x.edges ++ y.edges)
  }

  implicit def edgeFunctor[E, N] = new Functor[({ type f[x] = Edge[x, N] })#f] {
    override def map[A, B](fa: Edge[A, N])(f: A => B): Edge[B, N] = fa.map(f)
  }

  implicit def graphEdgeFunctor[N, E](implicit edgeFunctor: Functor[({ type f[x] = Edge[x, N] })#f]) = new Functor[({ type f[x] = Graph[N, x] })#f] {
    override def map[A, B](fa: Graph[N, A])(f: A => B): Graph[N, B] = fa match {
      case _: Graph[_, _] => Graph.empty
      case SomeGraph(nodes, edges) =>
        SomeGraph[N, B](nodes, edges.map(_.map(f)))
    }
  }
}
