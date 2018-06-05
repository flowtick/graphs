package com.flowtick.graphs

import com.flowtick.graphs.defaults._
import com.flowtick.graphs.defaults.directed._
import org.scalatest.{ FlatSpec, Matchers }

class GraphSpec extends FlatSpec with Matchers {

  val graph = DefaultGraph.create(Seq(
    n("A") -> n("B"),
    n("B") -> n("C"),
    n("C") -> n("D"),
    n("D") -> n("A"),
    n("A") -> n("C"),
    n("B") -> n("D")))

  "Graph" should "provide incoming edges for nodes" in {
    graph.incoming(n("A")).toList should contain theSameElementsAs List(
      DefaultEdge(n("D"), Some(n("A"))))

    graph.incoming(n("B")).toList should contain theSameElementsAs List(
      DefaultEdge(n("A"), Some(n("B"))))

    graph.incoming(n("C")).toList should contain theSameElementsAs List(
      DefaultEdge(n("B"), Some(n("C"))),
      DefaultEdge(n("A"), Some(n("C"))))

    graph.incoming(n("D")).toList should contain theSameElementsAs List(
      DefaultEdge(n("C"), Some(n("D"))),
      DefaultEdge(n("B"), Some(n("D"))))
  }

  "Graph" should "provide outgoing edges for nodes" in {
    graph.outgoing(n("A")).toList should contain theSameElementsAs List(
      DefaultEdge(n("A"), Some(n("B"))),
      DefaultEdge(n("A"), Some(n("C"))))

    graph.outgoing(n("B")).toList should contain theSameElementsAs List(
      DefaultEdge(n("B"), Some(n("C"))),
      DefaultEdge(n("B"), Some(n("D"))))

    graph.outgoing(n("C")).toList should contain theSameElementsAs List(
      DefaultEdge(n("C"), Some(n("D"))))

    graph.outgoing(n("D")).toList should contain theSameElementsAs List(
      DefaultEdge(n("D"), Some(n("A"))))
  }

}
