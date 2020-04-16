package com.flowtick.graphs

import org.scalacheck.Gen

object GraphGen {
  def edgesGen[E, N](implicit valueGen: Gen[E], nodeGen: Gen[N]): Gen[List[Edge[E, N]]] = for {
    lefts <- Gen.listOf(nodeGen)
    rights <- Gen.listOfN(lefts.size, nodeGen)
    values <- Gen.listOfN(lefts.size, valueGen)
  } yield lefts.zip(rights).zip(values).map {
    case ((left, right), value) => Edge(value, left, right)
  }

  def graphGen[E, N, M](implicit
    edgeGen: Gen[E],
    nodeGen: Gen[N],
    nodesGen: Gen[List[N]],
    metaGen: Gen[M]): Gen[Graph[E, N, M]] = for {
    meta <- metaGen
    nodes <- nodesGen
    edges <- edgesGen[E, N]
  } yield Graph(meta, edges = edges, nodes = nodes)
}
