package com.flowtick.graphs.cat

import com.flowtick.graphs.SomeGraph
import com.flowtick.graphs.defaults._
import com.flowtick.graphs.defaults.directed._
import org.scalatest.{ FlatSpec, Matchers }

class GraphMonoidSpec extends FlatSpec with Matchers {
  "Graph Monoid" should "combine graphs" in {
    val graphA = DefaultGraph.create(Seq(
      1 -> 2,
      2 -> 3))

    val graphB = DefaultGraph.create(Seq(
      2 -> 3,
      4 -> 3,
      4 -> 5))

    graphMonoid.combine(graphA, graphB) should be(SomeGraph[Int, DirectedEdge[Int]](
      Set(1, 2, 3, 4, 5),
      Set(
        DirectedEdge(1, Some(2)),
        DirectedEdge(4, Some(5)),
        DirectedEdge(2, Some(3)),
        DirectedEdge(4, Some(3)))))
  }
}
