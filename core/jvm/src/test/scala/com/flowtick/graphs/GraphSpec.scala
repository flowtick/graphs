package com.flowtick.graphs

import com.flowtick.graphs.defaults._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GraphSpec extends AnyFlatSpec with Matchers {

  val testGraph = Graph.fromEdges[Unit, String](
    Seq(
      "A" --> "B",
      "B" --> "C",
      "C" --> "D",
      "D" --> "A",
      "A" --> "C",
      "B" --> "D"
    )
  )

  "Graph" should "provide incoming edges for nodes" in {
    val incomingA = testGraph.incoming("A")
    incomingA should contain theSameElementsAs List(Edge.unit("D", "A"))

    val incomingB = testGraph.incoming("B")
    incomingB should contain theSameElementsAs List(Edge.unit("A", "B"))

    testGraph.incoming("C") should contain theSameElementsAs List(
      Edge.unit("B", "C"),
      Edge.unit("A", "C")
    )

    testGraph.incoming("D") should contain theSameElementsAs List(
      Edge.unit("C", "D"),
      Edge.unit("B", "D")
    )
  }

  it should "provide outgoing edges for nodes" in {
    testGraph.outgoing("A") should contain theSameElementsAs List(
      Edge.unit("A", "B"),
      Edge.unit("A", "C")
    )

    testGraph.outgoing("B") should contain theSameElementsAs List(
      Edge.unit("B", "C"),
      Edge.unit("B", "D")
    )

    testGraph.outgoing("C") should contain theSameElementsAs List(
      Edge.unit("C", "D")
    )

    testGraph.outgoing("D") should contain theSameElementsAs List(
      Edge.unit("D", "A")
    )
  }

  it should "get the predecessors for a node" in {
    testGraph.predecessors("A").toSet should be(Set(Node.of("D")))
  }

  it should "get the successors for a node" in {
    testGraph.successors("A").toSet should be(Set(Node.of("B"), Node.of("C")))
  }

  it should "return the all the nodes of a graph" in {
    testGraph.nodes should contain theSameElementsAs (List(
      Node.of("A"),
      Node.of("B"),
      Node.of("C"),
      Node.of("D")
    ))
  }

  it should "return the all the edges of a graph" in {
    testGraph.edges should contain theSameElementsAs (List(
      "A" --> "B",
      "B" --> "C",
      "C" --> "D",
      "D" --> "A",
      "A" --> "C",
      "B" --> "D"
    ).flatMap(_.toEdges))
  }

  it should "return empty iterable for an empty graph" in {
    val emptyGraph = Graph.empty[Unit, Int]
    emptyGraph.edges should be(empty)
  }

  it should "have nodes after adding an edge" in {
    val intGraph =
      Graph.empty[Option[Unit], Int].addEdge(None, 1, 2)
    intGraph.nodes should contain theSameElementsAs List(
      Node("1", 1),
      Node("2", 2)
    )
  }

  it should "remove nodes" in {
    val node1 = 1
    val node2 = 2
    val node3 = 3

    val intGraph = Graph
      .empty[Unit, Int]
      .addEdge((), node1, node2)
      .addNode(node3)

    intGraph.removeNodeValue(node3) should be(
      Graph.empty[Unit, Int].addEdge((), 1, 2)
    )

    val expected = Graph
      .empty[Unit, Int]
      .withNode(Node.of(2))
      .withNode(Node.of(3))

    intGraph.removeNodeValue(node1) should be(expected)
  }

  it should "remove edges" in {
    val intGraph = Graph
      .empty[Unit, Int]
      .addEdge((), 1, 2)
      .addNode(3)

    val expected = Graph
      .empty[Unit, Int]
      .withNode(Node.of(1))
      .withNode(Node.of(2))
      .withNode(Node.of(3))

    intGraph.edges.headOption match {
      case Some(edge) => intGraph.removeEdge(edge) should be(expected)
      case None       => fail("edge was not in the graph")
    }
  }

}
