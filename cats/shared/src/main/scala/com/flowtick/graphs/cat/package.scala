package com.flowtick.graphs

import cats.{ Contravariant, Functor, Monoid }

package object cat {

  class IdentifiableContravariant extends Contravariant[Identifiable] {
    override def contramap[A, B](fa: Identifiable[A])(f: B => A): Identifiable[B] =
      Identifiable.identify[B]((fa.id _).compose(f))
  }

  class GraphMonoid[G[_, _, _], E[_, _], V, N, M](implicit
    graph: Graph[G, E],
    builder: GraphBuilder[G, E],
    edge: EdgeType[E],
    metaMonoid: Monoid[M]) extends Monoid[G[E[V, N], N, M]] {
    override def empty: G[E[V, N], N, M] = builder.empty(metaMonoid.empty)

    override def combine(x: G[E[V, N], N, M], y: G[E[V, N], N, M]): G[E[V, N], N, M] =
      builder.withValue(metaMonoid.combine(graph.value(x), graph.value(y)))(graph.edges(x) ++ graph.edges(y), graph.nodes(x) ++ graph.nodes(y))
  }

  class EdgeValueFunctor[E[_, _], N](implicit edge: EdgeType[E]) extends Functor[({ type f[x] = E[x, N] })#f] {
    override def map[A, B](fa: E[A, N])(f: A => B): E[B, N] = edge.apply[B, N](f(edge.value(fa)), edge.head(fa), edge.tail(fa))
  }

  class EdgeNodeFunctor[E[_, _], V](implicit edge: EdgeType[E]) extends Functor[({ type f[x] = E[V, x] })#f] {
    override def map[A, B](fa: E[V, A])(f: A => B): E[V, B] = edge.apply(edge.value(fa), f(edge.head(fa)), f(edge.tail(fa)))
  }

  class GraphNodeFunctor[G[_, _, _], E[_, _], V, N, M](implicit
    graph: Graph[G, E],
    builder: GraphBuilder[G, E],
    edge: EdgeType[E],
    identifiable: Identifiable[N],
    edgeNodeFunctor: Functor[({ type f[x] = E[V, x] })#f]) extends Functor[({ type f[x] = G[E[V, x], x, M] })#f] {
    override def map[A, B](fa: G[E[V, A], A, M])(f: A => B): G[E[V, B], B, M] = {
      builder.build(
        graph.value(fa),
        graph.edges[V, A, M](fa).map(edgeNodeFunctor.map(_)(f)),
        graph.nodes[V, A, M](fa).map(f),
        graph.incoming[V, A, M](fa).map {
          case (node, incoming) => (f(node), incoming.map(edgeNodeFunctor.map(_)(f)))
        },
        graph.outgoing[V, A, M](fa).map {
          case (node, outgoing) => (f(node), outgoing.map(edgeNodeFunctor.map(_)(f)))
        })
    }
  }

  trait GraphInstances {
    implicit def graphMonoid[G[_, _, _], E[_, _], V, N, M](implicit
      graph: Graph[G, E],
      builder: GraphBuilder[G, E],
      edge: EdgeType[E],
      identifiable: Identifiable[N],
      metaMonoid: Monoid[M]) = new GraphMonoid[G, E, V, N, M]

    implicit def graphNodeFunctor[G[_, _, _], E[_, _], V, N, M](implicit
      graph: Graph[G, E],
      builder: GraphBuilder[G, E],
      edge: EdgeType[E],
      identifiable: Identifiable[N],
      edgeNodeFunctor: Functor[({ type f[x] = E[V, x] })#f]) = new GraphNodeFunctor[G, E, V, N, M]

    implicit def edgeNodeFunctor[E[_, _], V](implicit edge: EdgeType[E]): Functor[({ type f[x] = E[V, x] })#f] = new EdgeNodeFunctor[E, V]

    implicit def identifiableContravariant = new IdentifiableContravariant
  }

  object instances extends GraphInstances
}
