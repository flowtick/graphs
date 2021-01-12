package com.flowtick.graphs.algorithm

import com.flowtick.graphs.algorithm.Traversal.Step
import com.flowtick.graphs.{Edge, Graph, Node}
import com.flowtick.graphs.defaults._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DepthFirstSearchSpec extends AnyFlatSpec with Matchers {
  "Dfs" should "traverse in depth first manner" in {
    val graph = Graph.fromEdges(Seq(
      "1" --> "2",
      "1" --> "3",

      "2" --> "4",
      "2" --> "5",

      "3" --> "6",
      "3" --> "7"))

    val traversal = graph.dfs("1").run

    val values: Iterable[String] = traversal.collect {
      case Visited(step) => step.node.value
    }

    values should be(List("1", "3", "7", "6", "2", "5", "4"))

    val expected = List(
      Visited(Step(Node.of("1"), None)),
      Visited(Step(Node.of("3"), Some(Edge.unit("1", "3")))),
      Visited(Step(Node.of("7"), Some(Edge.unit("3", "7")))),
      Completed(Step(Node.of("7"), Some(Edge.unit("3", "7")))),
      Visited(Step(Node.of("6"), Some(Edge.unit("3", "6")))),
      Completed(Step(Node.of("6"), Some(Edge.unit("3", "6")))),
      Completed(Step(Node.of("3"), Some(Edge.unit("1", "3")))),
      Visited(Step(Node.of("2"), Some(Edge.unit("1", "2")))),
      Visited(Step(Node.of("5"), Some(Edge.unit("2", "5")))),
      Completed(Step(Node.of("5"), Some(Edge.unit("2", "5")))),
      Visited(Step(Node.of("4"), Some(Edge.unit("2", "4")))),
      Completed(Step(Node.of("4"), Some(Edge.unit("2", "4")))),
      Completed(Step(Node.of("2"), Some(Edge.unit("1", "2")))),
      Completed(Step(Node.of("1"), None))
    )

    traversal.toList should be(expected)
  }

  it should "traverse graphs with loops" in {
    val graph = Graph.fromEdges(Seq("1" ~~~ "2"))
    
    val traversal = graph.dfs("1").run

    traversal.toList should be(List(
      Visited(Step(Node.of("1"), None)),
      Visited(Step(Node.of("2"), Some(Edge.unit("1", "2")))),
      Completed(Step(Node.of("2"), Some(Edge.unit("1", "2")))),
      Completed(Step(Node.of("1"), None), backtrack = Some(Step(Node.of("2"), Some(Edge.unit("2", "1"))))
    )))
  }
}
