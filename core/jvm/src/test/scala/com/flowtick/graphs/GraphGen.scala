package com.flowtick.graphs

import org.scalacheck.Gen

object GraphGen {
  def edgesGen[ET[_, _], V, N](implicit edge: EdgeType[ET], valueGen: Gen[V], nodeGen: Gen[N]): Gen[List[ET[Option[V], N]]] = for {
    lefts <- Gen.listOf(nodeGen)
    rights <- Gen.listOfN(lefts.size, nodeGen)
    values <- Gen.listOfN(lefts.size, Gen.option(valueGen))
  } yield lefts.zip(rights).zip(values).map {
    case ((left, right), value) => edge.apply(value, left, right)
  }

  def graphGen[G[_, _, _], ET[_, _], V, N, M](implicit
    graph: Graph[G],
    builder: GraphBuilder[G],
    edge: EdgeType[ET],
    identifiable: Identifiable[N],
    metaGen: Gen[M],
    valueGen: Gen[V],
    nodeGen: Gen[N]): Gen[G[ET[Option[V], N], N, M]] = for {
    meta <- metaGen
    edges <- edgesGen[ET, V, N]
  } yield builder.of(meta)(edges: _*)

}
