package com.flowtick.graphs.algorithm

import com.flowtick.graphs._
import org.scalamock.scalatest.proxy.MockFactory
import org.scalatest.{ FlatSpec, Matchers }

class BreadthFirstSearchSpec extends FlatSpec with Matchers with MockFactory {
  "Bfs" should "traverse in breadth first manner" in {

    val graph = Graph[DefaultNode, DirectedEdge[DefaultNode]](
      n("A") ~> n("B"),
      n("A") ~> n("C"),
      n("A") ~> n("D"),

      n("B") ~> n("E"),
      n("B") ~> n("F"),
      n("B") ~> n("G"),

      n("E") ~> n("H")
    )

    val visitMock = mockFunction[DefaultNode, Unit](functionName("visitCallback"))
    val completeMock = mockFunction[DefaultNode, Unit](functionName("completeCallback"))

    inSequence {
      visitMock.expects(n("A"))
      completeMock.expects(n("A"))
      visitMock.expects(n("D"))
      visitMock.expects(n("C"))
      visitMock.expects(n("B"))
      completeMock.expects(n("D"))
      completeMock.expects(n("C"))
      completeMock.expects(n("B"))
      visitMock.expects(n("E"))
      visitMock.expects(n("F"))
      visitMock.expects(n("G"))
      completeMock.expects(n("E"))
      visitMock.expects(n("H"))
      completeMock.expects(n("F"))
      completeMock.expects(n("G"))
      completeMock.expects(n("H"))
    }

    graph.bfs(startNode = Some(n("A"))).onVisit(node => {
      println(s"visit: $node")
      visitMock(node)
    }).onComplete(node => {
      println(s"completed: $node")
      completeMock(node)
    }).run
  }
}
