package com.flowtick.graphs.algorithm

import com.flowtick.graphs.defaults._
import com.flowtick.graphs.defaults.directed._
import org.scalamock.scalatest.proxy.MockFactory
import org.scalatest.{ FlatSpec, Matchers }

class BreadthFirstSearchSpec extends FlatSpec with Matchers with MockFactory {
  "Bfs" should "traverse in breadth first manner" in {

    val graph = DefaultGraph.create(Seq(
      n("A") -> n("D"),
      n("A") -> n("C"),
      n("A") -> n("B"),
      n("B") -> n("E"),
      n("B") -> n("F"),
      n("B") -> n("G"),
      n("E") -> n("H")))

    val visitMock = mockFunction[DefaultNode, Unit](functionName("visitCallback"))
    val completeMock = mockFunction[DefaultNode, Unit](functionName("completeCallback"))

    inSequence {
      visitMock.expects(n("A"))
      visitMock.expects(n("C"))
      visitMock.expects(n("D"))
      visitMock.expects(n("B"))
      completeMock.expects(n("A"))
      completeMock.expects(n("C"))
      completeMock.expects(n("D"))
      visitMock.expects(n("G"))
      visitMock.expects(n("E"))
      visitMock.expects(n("F"))
      completeMock.expects(n("B"))
      completeMock.expects(n("G"))
      visitMock.expects(n("H"))
      completeMock.expects(n("E"))
      completeMock.expects(n("F"))
      completeMock.expects(n("H"))
    }

    graph.bfs(n("A")).onVisit(node => {
      visitMock(node)
    }).onComplete(node => {
      completeMock(node)
    }).run
  }
}
