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

    graphMonoid.combine(graphA, graphB) should be(SomeGraph[Int, DefaultEdge[Int]](
      Set(1, 2, 3, 4, 5),
      Set(
        DefaultEdge(1, Some(2), true),
        DefaultEdge(4, Some(5), true),
        DefaultEdge(2, Some(3), true),
        DefaultEdge(4, Some(3), true))))
  }
}
