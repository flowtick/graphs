package com.flowtick.graphs.cat

import com.flowtick.graphs._
import com.flowtick.graphs.defaults._
import cats.implicits._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GraphCatsSpec extends AnyFlatSpec with Matchers {
  import com.flowtick.graphs.cat.instances._

  "Graph Monoid" should "combine graphs" in {
    type NumberNodeGraph = Graph[Unit, Int]

    val graphA: NumberNodeGraph = Graph.fromEdges(Set(
      1 --> 2,
      2 --> 3)).withNode(Node.of(10))

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
      5 --> 1).map(_.toEdge)

    combined.nodes.map(_.value) should contain theSameElementsAs Seq(
      1,
      2,
      3,
      4,
      5,
      10
    )
  }

  "Graph Applicative" should "map over nodes" in {
    val numberGraph: Graph[Unit, Int] = Graph.fromEdges[Unit, Int](Set(1 --> 2))
    val doubledNodes: Graph[Unit, Int] = numberGraph.map(_ * 2)

    doubledNodes.findNode("1").map(_.value) should be(Some(2))
    doubledNodes.findNode("2").map(_.value) should be(Some(4))
  }

}
