package com.flowtick.graphs.algorithm

import com.flowtick.graphs.{Graph, Node}
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
      case Visited(node) => node.value
    }

    values should be(List("1", "3", "7", "6", "2", "5", "4"))

    val expected = List(
      Visited(Node.of("1")),
      Visited(Node.of("3")),
      Visited(Node.of("7")),
      Completed(Node.of("7")),
      Visited(Node.of("6")),
      Completed(Node.of("6")),
      Completed(Node.of("3")),
      Visited(Node.of("2")),
      Visited(Node.of("5")),
      Completed(Node.of("5")),
      Visited(Node.of("4")),
      Completed(Node.of("4")),
      Completed(Node.of("2")),
      Completed(Node.of("1"))
    )

    traversal.toList should be(expected)
  }
}
