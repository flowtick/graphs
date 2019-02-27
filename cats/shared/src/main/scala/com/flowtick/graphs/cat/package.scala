package com.flowtick.graphs

import cats.{ Functor, Monoid }

package object cat {

  implicit def graphMonoid[G[_, _, _], E[_, _], V, N, M](implicit
    graph: Graph[G, E],
    builder: GraphBuilder[G, E],
    edge: EdgeType[E],
    identifiable: Identifiable[N],
    metaEmpty: Empty[M],
    metaMonoid: Monoid[M]) = new Monoid[G[E[V, N], N, M]] {
    override def empty: G[E[V, N], N, M] = builder.empty

    override def combine(x: G[E[V, N], N, M], y: G[E[V, N], N, M]): G[E[V, N], N, M] =
      builder.withValue(metaMonoid.combine(graph.value(x), graph.value(y)))(graph.edges(x) ++ graph.edges(y))
  }

  implicit def edgeFunctor[E[_, _], N](implicit edge: EdgeType[E]) = new Functor[({ type f[x] = E[x, N] })#f] {
    override def map[A, B](fa: E[A, N])(f: A => B): E[B, N] = edge.apply[B, N](f(edge.value(fa)), edge.head(fa), edge.tail(fa))
  }

  /*implicit def graphNodeFunctor[N, E](implicit edgeFunctor: Functor[({ type f[x] = Edge[x, N] })#f]) = new Functor[({ type f[x] = Graph[N, x] })#f] {
    override def map[A, B](fa: Graph[N, A])(f: A => B): Graph[N, B] = fa match {
      case _: Graph[_, _] => Graph.empty
      case SomeGraph(nodes, edges) =>
        SomeGraph[N, B](nodes, edges.map(_.map(f)))
    }
  }*/
}
