package com.flowtick.graphs.algorithm

import com.flowtick.graphs.algorithm.Traversal.Step
import com.flowtick.graphs.defaults._
import com.flowtick.graphs.{Edge, Graph, Node}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DepthFirstSearchSpec extends AnyFlatSpec with Matchers {
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

  "Dfs" should "traverse in depth first manner" in {
    val traversal = graph.dfs("1").run

    val values: Iterable[String] = traversal.collect { case Visited(step) =>
      step.node.value
    }

    values should be(List("1", "3", "7", "6", "2", "5", "4"))

    val expected = List(
      Visited(Step(Node.of("1"), None, Some(0))),
      Visited(Step(Node.of("3"), Some(Edge.unit("1", "3")), Some(1))),
      Visited(Step(Node.of("7"), Some(Edge.unit("3", "7")), Some(2))),
      Completed(Step(Node.of("7"), Some(Edge.unit("3", "7")), Some(2))),
      Visited(Step(Node.of("6"), Some(Edge.unit("3", "6")), Some(2))),
      Completed(Step(Node.of("6"), Some(Edge.unit("3", "6")), Some(2))),
      Completed(Step(Node.of("3"), Some(Edge.unit("1", "3")), Some(1))),
      Visited(Step(Node.of("2"), Some(Edge.unit("1", "2")), Some(1))),
      Visited(Step(Node.of("5"), Some(Edge.unit("2", "5")), Some(2))),
      Completed(Step(Node.of("5"), Some(Edge.unit("2", "5")), Some(2))),
      Visited(Step(Node.of("4"), Some(Edge.unit("2", "4")), Some(2))),
      Completed(Step(Node.of("4"), Some(Edge.unit("2", "4")), Some(2))),
      Completed(Step(Node.of("2"), Some(Edge.unit("1", "2")), Some(1))),
      Completed(Step(Node.of("1"), None, Some(0)))
    )

    traversal.toList should be(expected)
  }

  it should "traverse graphs with loops" in {
    val graph = Graph.fromEdges(Seq("1" ~~~ "2"))

    val traversal = graph.dfs("1").run

    traversal.toList should be(
      List(
        Visited(Step(Node.of("1"), None, Some(0))),
        Visited(Step(Node.of("2"), Some(Edge.unit("1", "2")), Some(1))),
        Completed(Step(Node.of("2"), Some(Edge.unit("1", "2")), Some(1))),
        Completed(
          Step(Node.of("1"), None, Some(0)),
          backtrack = Some(Step(Node.of("2"), Some(Edge.unit("2", "1")), Some(1)))
        )
      )
    )
  }

  it should "retrieve all paths" in {
    graph.paths("1").map(_.steps.map(_.node.id)) should be(
      List(
        List("1"),
        List("1", "2"),
        List("1", "2", "4"),
        List("1", "2", "5"),
        List("1", "3"),
        List("1", "3", "6"),
        List("1", "3", "7")
      )
    )
  }
}
