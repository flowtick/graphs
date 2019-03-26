package com.flowtick.graphs

import cats.{ Contravariant, Functor, Monoid }

package object cat {

  class IdentifiableContravariant extends Contravariant[Identifiable] {
    override def contramap[A, B](fa: Identifiable[A])(f: B => A): Identifiable[B] =
      Identifiable.identify[B]((fa.id _).compose(f))
  }

  class GraphMonoid[G[_, _, _], V, N, M](implicit
    graph: Graph[G],
    builder: GraphBuilder[G],
    metaMonoid: Monoid[M]) extends Monoid[G[V, N, M]] {
    override def empty: G[V, N, M] = builder.empty(metaMonoid.empty)

    override def combine(x: G[V, N, M], y: G[V, N, M]): G[V, N, M] =
      builder.withValue(metaMonoid.combine(graph.value(x), graph.value(y)))(graph.edges(x) ++ graph.edges(y), graph.nodes(x) ++ graph.nodes(y))
  }

  class EdgeValueFunctor[N] extends Functor[({ type f[x] = Edge[x, N] })#f] {
    override def map[A, B](fa: Edge[A, N])(f: A => B): Edge[B, N] = Edge(f(fa.value), fa.head, fa.tail)
  }

  class EdgeNodeFunctor[V] extends Functor[({ type f[x] = Edge[V, x] })#f] {
    override def map[A, B](fa: Edge[V, A])(f: A => B): Edge[V, B] = Edge(fa.value, f(fa.head), f(fa.tail))
  }

  class GraphNodeFunctor[G[_, _, _], V, N, M](implicit
    graph: Graph[G],
    builder: GraphBuilder[G],
    identifiable: Identifiable[N],
    edgeNodeFunctor: Functor[({ type f[x] = Edge[V, x] })#f]) extends Functor[({ type f[x] = G[V, x, M] })#f] {
    override def map[A, B](fa: G[V, A, M])(f: A => B): G[V, B, M] = {
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
    implicit def graphMonoid[G[_, _, _], V, N, M](implicit
      graph: Graph[G],
      builder: GraphBuilder[G],
      identifiable: Identifiable[N],
      metaMonoid: Monoid[M]) = new GraphMonoid[G, V, N, M]

    implicit def graphNodeFunctor[G[_, _, _], V, N, M](implicit
      graph: Graph[G],
      builder: GraphBuilder[G],
      identifiable: Identifiable[N],
      edgeNodeFunctor: Functor[({ type f[x] = Edge[V, x] })#f]) = new GraphNodeFunctor[G, V, N, M]

    implicit def edgeNodeFunctor[V]: Functor[({ type f[x] = Edge[V, x] })#f] = new EdgeNodeFunctor[V]

    implicit def identifiableContravariant = new IdentifiableContravariant
  }

  object instances extends GraphInstances
}
