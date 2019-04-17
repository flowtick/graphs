package com.flowtick.graphs

import com.flowtick.graphs.defaults._
import org.scalatest.{ FlatSpec, Matchers }

class GraphSpec extends FlatSpec with Matchers {
  val testGraph: Graph[Unit, String, Unit] = Graph.from(Seq(
    n("A") --> n("B"),
    n("B") --> n("C"),
    n("C") --> n("D"),
    n("D") --> n("A"),
    n("A") --> n("C"),
    n("B") --> n("D")))

  "Graph" should "provide incoming edges for nodes" in {
    testGraph.incoming("A") should contain theSameElementsAs List(
      n("D") --> n("A"))

    testGraph.incoming("B") should contain theSameElementsAs List(
      n("A") --> n("B"))

    testGraph.incoming("C") should contain theSameElementsAs List(
      n("B") --> n("C"),
      n("A") --> n("C"))

    testGraph.incoming("D") should contain theSameElementsAs List(
      n("C") --> n("D"),
      n("B") --> n("D"))
  }

  it should "provide outgoing edges for nodes" in {
    testGraph.outgoing("A") should contain theSameElementsAs List(
      n("A") --> n("B"),
      n("A") --> n("C"))

    testGraph.outgoing("B") should contain theSameElementsAs List(
      n("B") --> n("C"),
      n("B") --> n("D"))

    testGraph.outgoing("C") should contain theSameElementsAs List(
      n("C") --> n("D"))

    testGraph.outgoing("D") should contain theSameElementsAs List(
      n("D") --> n("A"))
  }

  it should "get the predecessors for a node" in {
    testGraph.predecessors("A").toList should be(List("D"))
  }

  it should "get the successors for a node" in {
    testGraph.successors("A").toList should be(List("B", "C"))
  }

}