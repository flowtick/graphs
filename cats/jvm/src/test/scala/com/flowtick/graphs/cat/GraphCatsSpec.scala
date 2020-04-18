package com.flowtick.graphs.cat

import com.flowtick.graphs._
import com.flowtick.graphs.defaults._
import cats.implicits._

import org.scalatest.{ FlatSpec, Matchers }

class GraphCatsSpec extends FlatSpec with Matchers {
  import com.flowtick.graphs.cat.instances._

  "Graph Monoid" should "combine graphs" in {
    type NumberNodeGraph = Graph[Unit, Unit, Int]

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

  it should "combine meta" in {
    val a = Graph(List("foo"), edges = Set(1 --> 2))
    val b = Graph(List("bar"), edges = Set(2 --> 3))

    a |+| b should be(Graph(List("foo", "bar"), edges = Set(1 --> 2, 2 --> 3)))
  }

  "Graph Applicative" should "map over nodes" in {
    val numberGraph: Graph[Unit, Unit, Int] = Graph.fromEdges[Unit, Int](Set(1 --> 2))
    val doubledNodes: Graph[Unit, Unit, Int] = numberGraph.map(_ * 2)

    doubledNodes should be(Graph.fromEdges[Unit, Int](Set(2 --> 4)))
  }

  it should "retain meta" in {
    val numberGraph: Graph[List[String], Unit, Int] = Graph(List("foo"), edges = Set(1 --> 2)).withNode(5)
    val doubledNodes = numberGraph.map(_ * 2)

    doubledNodes should be(Graph[List[String], Unit, Int](List("foo"), edges = Set(2 --> 4)).withNode(10))
  }

}
