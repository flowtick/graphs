package com.flowtick.graphs.algorithm

import com.flowtick.graphs.algorithm.Traversal.Step
import com.flowtick.graphs.{Edge, Graph, Node}
import com.flowtick.graphs.defaults._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BreadthFirstSearchSpec extends AnyFlatSpec with Matchers {
  "Bfs" should "traverse in breadth first manner" in {

    val graph: Graph[Unit, String] = Graph.fromEdges(
      Seq(
        "1" --> "2",
        "1" --> "3",
        "2" --> "4",
        "2" --> "5",
        "3" --> "6",
        "3" --> "7"
      )
    )

    val traversal = graph.bfs("1").run

    val values: Iterable[String] = traversal.collect { case Completed(step, _) =>
      step.node.value
    }

    values should be(List("1", "2", "3", "4", "5", "6", "7"))

    val expected = List(
      Visited(Step(Node.of("1"), None, Some(0))),
      Visited(Step(Node.of("2"), Some(Edge.unit("1", "2")), Some(1))),
      Visited(Step(Node.of("3"), Some(Edge.unit("1", "3")), Some(1))),
      Completed(Step(Node.of("1"), None, Some(0))),
      Visited(Step(Node.of("4"), Some(Edge.unit("2", "4")), Some(2))),
      Visited(Step(Node.of("5"), Some(Edge.unit("2", "5")), Some(2))),
      Completed(Step(Node.of("2"), Some(Edge.unit("1", "2")), Some(1))),
      Visited(Step(Node.of("6"), Some(Edge.unit("3", "6")), Some(2))),
      Visited(Step(Node.of("7"), Some(Edge.unit("3", "7")), Some(2))),
      Completed(Step(Node.of("3"), Some(Edge.unit("1", "3")), Some(1))),
      Completed(Step(Node.of("4"), Some(Edge.unit("2", "4")), Some(2))),
      Completed(Step(Node.of("5"), Some(Edge.unit("2", "5")), Some(2))),
      Completed(Step(Node.of("6"), Some(Edge.unit("3", "6")), Some(2))),
      Completed(Step(Node.of("7"), Some(Edge.unit("3", "7")), Some(2)))
    )

    traversal.toList should be(expected)
  }
}
