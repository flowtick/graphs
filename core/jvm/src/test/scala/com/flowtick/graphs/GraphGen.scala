package com.flowtick.graphs

import org.scalacheck.Gen
import defaults._
import defaults.anyId._

object GraphGen {
  def edgesGen[E, N](implicit valueGen: Gen[E], nodeGen: Gen[N]): Gen[List[Edge[E]]] = for {
    lefts <- Gen.listOf(nodeGen)
    rights <- Gen.listOfN(lefts.size, nodeGen)
    values <- Gen.listOfN(lefts.size, valueGen)
  } yield lefts.zip(rights).zip(values).map {
    case ((left, right), value) => Edge.of(value, left.toString, right.toString)
  }

  def graphGen[M, E, N](implicit
    edgeGen: Gen[E],
    nodeGen: Gen[N],
    nodesGen: Gen[List[N]],
    metaGen: Gen[M]): Gen[Graph[E, N]] = for {
    nodes <- nodesGen
    edges <- edgesGen[E, N]
  } yield Graph(edges = edges, nodes = nodes.map(Node.of))
}
