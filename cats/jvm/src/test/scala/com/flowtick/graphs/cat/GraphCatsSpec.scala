package com.flowtick.graphs.cat

import com.flowtick.graphs._
import com.flowtick.graphs.defaults._
import cats.implicits._

import org.scalatest.{ FlatSpec, Matchers }

class GraphCatsSpec extends FlatSpec with Matchers {
  import com.flowtick.graphs.cat.instances._

  "Graph Monoid" should "combine graphs" in {
    type NumberNodeGraph = Graph[Unit, Int]

    val graphA: NumberNodeGraph = Graph.fromEdges(Set(
      1 --> 2,
      2 --> 3)).withNode(10)

    val graphB: NumberNodeGraph = Graph.fromEdges(Set(
      2 --> 3,
      4 --> 3,
      4 --> 5,
      5 --> 1))

    val combined = graphA |+| graphB

    combined.edges should contain theSameElementsAs Seq(
      1 --> 2,
      2 --> 3,
      4 --> 3,
      4 --> 5,
      5 --> 1)

    combined.nodes should contain theSameElementsAs Seq(
      1,
      2,
      3,
      4,
      5,
      10
    )
  }

}
