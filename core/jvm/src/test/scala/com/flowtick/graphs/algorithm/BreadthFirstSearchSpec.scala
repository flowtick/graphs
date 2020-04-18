package com.flowtick.graphs.algorithm

import com.flowtick.graphs.Graph
import com.flowtick.graphs.defaults._
import org.scalamock.scalatest.proxy.MockFactory
import org.scalatest.{ FlatSpec, Matchers }

class BreadthFirstSearchSpec extends FlatSpec with Matchers with MockFactory {
  "Bfs" should "traverse in breadth first manner" in {

    val graph = Graph.fromEdges(Seq(
      "1" --> "2",
      "1" --> "3",

      "2" --> "4",
      "2" --> "5",

      "3" --> "6",
      "3" --> "7"))

    val visitMock = mockFunction[String, Unit](functionName("visitCallback"))
    val completeMock = mockFunction[String, Unit](functionName("completeCallback"))

    inSequence {
      visitMock.expects("1")
      visitMock.expects("2")
      visitMock.expects("3")
      completeMock.expects("1")

      visitMock.expects("4")
      visitMock.expects("5")
      completeMock.expects("2")

      visitMock.expects("6")
      visitMock.expects("7")
      completeMock.expects("3")
      completeMock.expects("4")
      completeMock.expects("5")
      completeMock.expects("6")
      completeMock.expects("7")
    }

    val bfsResult = graph.bfs("1").onVisit(node => {
      visitMock(node)
    }).onComplete(node => {
      completeMock(node)
    }).run

    bfsResult should be(List("1", "2", "3", "4", "5", "6", "7"))
  }
}
