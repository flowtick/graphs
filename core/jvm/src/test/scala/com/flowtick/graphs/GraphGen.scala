package com.flowtick.graphs

import org.scalacheck.Gen

object GraphGen {
  def edgesGen[V, N](implicit valueGen: Gen[V], nodeGen: Gen[N]): Gen[List[Edge[Option[V], N]]] = for {
    lefts <- Gen.listOf(nodeGen)
    rights <- Gen.listOfN(lefts.size, nodeGen)
    values <- Gen.listOfN(lefts.size, Gen.option(valueGen))
  } yield lefts.zip(rights).zip(values).map {
    case ((left, right), value) => Edge(value, left, right)
  }

  def graphGen[G[_, _, _], V, N, M](implicit
    graph: Graph[G],
    builder: GraphBuilder[G],
    identifiable: Identifiable[N],
    metaGen: Gen[M],
    valueGen: Gen[V],
    nodeGen: Gen[N]): Gen[G[Option[V], N, M]] = for {
    meta <- metaGen
    edges <- edgesGen[V, N]
  } yield builder.of(meta)(edges: _*)

}
