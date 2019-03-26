package com.flowtick.graphs

import com.flowtick.graphs.defaults._
import com.flowtick.graphs.defaults.directed._
import org.scalatest.{ FlatSpec, Matchers }

class GraphSpec extends FlatSpec with Matchers {
  val testGraph: DefaultGraph[Unit, String, Unit] = directedGraph.from(Seq(
    n("A") --> n("B"),
    n("B") --> n("C"),
    n("C") --> n("D"),
    n("D") --> n("A"),
    n("A") --> n("C"),
    n("B") --> n("D")))

  "Graph" should "provide incoming edges for nodes" in {
    directedGraph.incoming(testGraph).getOrElse("A", Iterable.empty) should contain theSameElementsAs List(
      n("D") --> n("A"))

    directedGraph.incoming(testGraph).getOrElse("B", Iterable.empty) should contain theSameElementsAs List(
      n("A") --> n("B"))

    directedGraph.incoming(testGraph).getOrElse("C", Iterable.empty) should contain theSameElementsAs List(
      n("B") --> n("C"),
      n("A") --> n("C"))

    directedGraph.incoming(testGraph).getOrElse("D", Iterable.empty) should contain theSameElementsAs List(
      n("C") --> n("D"),
      n("B") --> n("D"))
  }

  it should "provide outgoing edges for nodes" in {
    directedGraph.outgoing(testGraph).getOrElse("A", Iterable.empty) should contain theSameElementsAs List(
      n("A") --> n("B"),
      n("A") --> n("C"))

    directedGraph.outgoing(testGraph).getOrElse("B", Iterable.empty) should contain theSameElementsAs List(
      n("B") --> n("C"),
      n("B") --> n("D"))

    directedGraph.outgoing(testGraph).getOrElse("C", Iterable.empty) should contain theSameElementsAs List(
      n("C") --> n("D"))

    directedGraph.outgoing(testGraph).getOrElse("D", Iterable.empty) should contain theSameElementsAs List(
      n("D") --> n("A"))
  }

  it should "get the predecessors for a node" in {
    directedGraph.predecessors("A", testGraph).toList should be(List("D"))
  }

  it should "get the successors for a node" in {
    directedGraph.successors("A", testGraph).toList should be(List("B", "C"))
  }

}