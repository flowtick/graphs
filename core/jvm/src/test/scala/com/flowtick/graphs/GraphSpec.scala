package com.flowtick.graphs

import com.flowtick.graphs.defaults._
import com.flowtick.graphs.defaults.directed._
import org.scalatest.{ FlatSpec, Matchers }

class GraphSpec extends FlatSpec with Matchers {
  val testGraph: DefaultGraph[Edge[Unit, String], String, Unit] = defaultGraph.from(Seq(
    n("A") --> n("B"),
    n("B") --> n("C"),
    n("C") --> n("D"),
    n("D") --> n("A"),
    n("A") --> n("C"),
    n("B") --> n("D")))

  "Graph" should "provide incoming edges for nodes" in {
    defaultGraph.incoming(testGraph).getOrElse("A", Iterable.empty) should contain theSameElementsAs List(
      n("D") --> n("A"))

    defaultGraph.incoming(testGraph).getOrElse("B", Iterable.empty) should contain theSameElementsAs List(
      n("A") --> n("B"))

    defaultGraph.incoming(testGraph).getOrElse("C", Iterable.empty) should contain theSameElementsAs List(
      n("B") --> n("C"),
      n("A") --> n("C"))

    defaultGraph.incoming(testGraph).getOrElse("D", Iterable.empty) should contain theSameElementsAs List(
      n("C") --> n("D"),
      n("B") --> n("D"))
  }

  it should "provide outgoing edges for nodes" in {
    defaultGraph.outgoing(testGraph).getOrElse("A", Iterable.empty) should contain theSameElementsAs List(
      n("A") --> n("B"),
      n("A") --> n("C"))

    defaultGraph.outgoing(testGraph).getOrElse("B", Iterable.empty) should contain theSameElementsAs List(
      n("B") --> n("C"),
      n("B") --> n("D"))

    defaultGraph.outgoing(testGraph).getOrElse("C", Iterable.empty) should contain theSameElementsAs List(
      n("C") --> n("D"))

    defaultGraph.outgoing(testGraph).getOrElse("D", Iterable.empty) should contain theSameElementsAs List(
      n("D") --> n("A"))
  }

  it should "get the predecessors for a node" in {
    defaultGraph.predecessors("A", testGraph).toList should be(List("D"))
  }

  it should "get the successors for a node" in {
    defaultGraph.successors("A", testGraph).toList should be(List("B", "C"))
  }

}