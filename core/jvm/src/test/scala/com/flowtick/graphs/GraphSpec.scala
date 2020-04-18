package com.flowtick.graphs

import com.flowtick.graphs.defaults._

import org.scalatest.{ FlatSpec, Matchers }

class GraphSpec extends FlatSpec with Matchers {
  
  val testGraph = Graph.fromEdges[Unit, String](Seq(
    "A" --> "B",
    "B" --> "C",
    "C" --> "D",
    "D" --> "A",
    "A" --> "C",
    "B" --> "D"))

  "Graph" should "provide incoming edges for nodes" in {
    val incomingA = testGraph.incoming("A")
    incomingA should contain theSameElementsAs List(
      "D" --> "A")

    val incomingB = testGraph.incoming("B")
    incomingB should contain theSameElementsAs List(
      "A" --> "B")

    testGraph.incoming("C") should contain theSameElementsAs List(
      "B" --> "C",
      "A" --> "C")

    testGraph.incoming("D") should contain theSameElementsAs List(
      "C" --> "D",
      "B" --> "D")
  }

  it should "provide outgoing edges for nodes" in {
    testGraph.outgoing("A") should contain theSameElementsAs List(
      "A" --> "B",
      "A" --> "C")

    testGraph.outgoing("B") should contain theSameElementsAs List(
      "B" --> "C",
      "B" --> "D")

    testGraph.outgoing("C") should contain theSameElementsAs List(
      "C" --> "D")

    testGraph.outgoing("D") should contain theSameElementsAs List(
      "D" --> "A")
  }

  it should "get the predecessors for a node" in {
    testGraph.predecessors("A").toSet should be(Set("D"))
  }

  it should "get the successors for a node" in {
    testGraph.successors("A").toSet should be(Set("B", "C"))
  }

  it should "return the all the nodes of a graph" in {
    testGraph.nodes should be(Set("A", "B", "C", "D"))
  }

  it should "return empty iterable for an empty graph" in {
    val emptyGraph = Graph.unit
    emptyGraph.edges should be(empty)
  }

}