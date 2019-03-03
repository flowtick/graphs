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
    defaultGraph.incoming("A", testGraph) should contain theSameElementsAs List(
      n("D") --> n("A"))

    defaultGraph.incoming("B", testGraph) should contain theSameElementsAs List(
      n("A") --> n("B"))

    defaultGraph.incoming("C", testGraph) should contain theSameElementsAs List(
      n("B") --> n("C"),
      n("A") --> n("C"))

    defaultGraph.incoming("D", testGraph) should contain theSameElementsAs List(
      n("C") --> n("D"),
      n("B") --> n("D"))
  }

  "Graph" should "provide outgoing edges for nodes" in {
    defaultGraph.outgoing("A", testGraph) should contain theSameElementsAs List(
      n("A") --> n("B"),
      n("A") --> n("C"))

    defaultGraph.outgoing("B", testGraph) should contain theSameElementsAs List(
      n("B") --> n("C"),
      n("B") --> n("D"))

    defaultGraph.outgoing("C", testGraph) should contain theSameElementsAs List(
      n("C") --> n("D"))

    defaultGraph.outgoing("D", testGraph) should contain theSameElementsAs List(
      n("D") --> n("A"))
  }

}