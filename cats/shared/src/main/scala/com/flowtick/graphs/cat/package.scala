package com.flowtick.graphs

import cats.{ Contravariant, Functor, Monoid }

package object cat {

  class IdentifiableContravariant extends Contravariant[Identifiable] {
    override def contramap[A, B](fa: Identifiable[A])(f: B => A): Identifiable[B] =
      Identifiable.identify[B]((fa.id _).compose(f))
  }

  class ContextMonoid[N, V] extends Monoid[scala.collection.Map[N, NodeContext[V, N]]] {
    override def empty: collection.Map[N, NodeContext[V, N]] = Map.empty

    override def combine(x: collection.Map[N, NodeContext[V, N]], y: collection.Map[N, NodeContext[V, N]]): collection.Map[N, NodeContext[V, N]] =
      (for {
        key <- (x.keysIterator ++ y.keysIterator).toIterable
      } yield {
        val xContext: NodeContext[V, N] = x.getOrElse(key, NodeContext.empty)
        val yContext: NodeContext[V, N] = y.getOrElse(key, NodeContext.empty)

        (key, NodeContext(xContext.incoming ++ yContext.incoming, xContext.outgoing ++ yContext.outgoing))
      }).toMap
  }

  class GraphMonoid[V, N, M](implicit metaMonoid: Monoid[M], contextMonoid: Monoid[scala.collection.Map[N, NodeContext[V, N]]]) extends Monoid[Graph[V, N, M]] {
    override def empty: Graph[V, N, M] = Graph.empty(metaMonoid.empty)

    override def combine(x: Graph[V, N, M], y: Graph[V, N, M]): Graph[V, N, M] =
      Graph[V, N, M](metaMonoid.combine(x.value, y.value), contextMonoid.combine(x.nodeContext, y.nodeContext))
  }

  class EdgeValueFunctor[N] extends Functor[({ type f[x] = Edge[x, N] })#f] {
    override def map[A, B](fa: Edge[A, N])(f: A => B): Edge[B, N] = Edge(f(fa.value), fa.head, fa.tail)
  }

  class EdgeNodeFunctor[V] extends Functor[({ type f[x] = Edge[V, x] })#f] {
    override def map[A, B](fa: Edge[V, A])(f: A => B): Edge[V, B] = Edge(fa.value, f(fa.head), f(fa.tail))
  }

  class NodeContextFunctor[V, N](implicit edgeNodeFunctor: Functor[({ type f[x] = Edge[V, x] })#f]) extends Functor[({ type f[x] = NodeContext[V, x] })#f] {
    override def map[A, B](fa: NodeContext[V, A])(f: A => B): NodeContext[V, B] =
      NodeContext(fa.incoming.map(edgeNodeFunctor.map(_)(f)), fa.outgoing.map(edgeNodeFunctor.map(_)(f)))
  }

  class GraphNodeFunctor[V, N, M](implicit
    identifiable: Identifiable[N],
    nodeContextFunctor: Functor[({ type f[x] = NodeContext[V, x] })#f]) extends Functor[({ type f[x] = Graph[V, x, M] })#f] {
    override def map[A, B](fa: Graph[V, A, M])(f: A => B): Graph[V, B, M] = {
      Graph[V, B, M](
        fa.value,
        fa.nodeContext.map {
          case (node, context) => (f(node), nodeContextFunctor.map(context)(f))
        })
    }
  }

  trait GraphInstances {
    implicit def contextMonoid[V, N] = new ContextMonoid[V, N]

    implicit def nodeContextFunctor[V, N] = new NodeContextFunctor[V, N]

    implicit def graphMonoid[V, N, M](implicit
      identifiable: Identifiable[N],
      metaMonoid: Monoid[M]) = new GraphMonoid[V, N, M]

    implicit def graphNodeFunctor[V, N, M](implicit
      identifiable: Identifiable[N],
      edgeNodeFunctor: Functor[({ type f[x] = Edge[V, x] })#f]) = new GraphNodeFunctor[V, N, M]

    implicit def edgeNodeFunctor[V]: Functor[({ type f[x] = Edge[V, x] })#f] = new EdgeNodeFunctor[V]

    implicit def identifiableContravariant = new IdentifiableContravariant
  }

  object instances extends GraphInstances
}
