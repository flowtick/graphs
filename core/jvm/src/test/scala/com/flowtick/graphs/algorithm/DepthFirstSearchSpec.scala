package com.flowtick.graphs.algorithm

import com.flowtick.graphs._
import com.flowtick.graphs.defaults._
import org.scalamock.scalatest.proxy.MockFactory
import org.scalatest.{FlatSpec, Matchers}

class DepthFirstSearchSpec extends FlatSpec with Matchers with MockFactory {
  "Dfs" should "traverse in depth first manner" in {

    val graph = Graph.create[DefaultNode, Edge[DefaultNode]] { implicit graph =>
      n("1") ~> n("2")
      n("1") ~> n("9")

      n("2") ~> n("3")
      n("2") ~> n("6")

      n("3") ~> n("4")
      n("3") ~> n("5")

      n("6") ~> n("7")
      n("6") ~> n("8")
    }

    val visitMock = mockFunction[DefaultNode, Unit](functionName("visitCallback"))
    val completeMock = mockFunction[DefaultNode, Unit](functionName("completeCallback"))

    inSequence {
      visitMock.expects(n("1"))
      visitMock.expects(n("9"))
      completeMock.expects(n("9"))
      visitMock.expects(n("2"))
      visitMock.expects(n("3"))
      visitMock.expects(n("4"))
      completeMock.expects(n("4"))
      visitMock.expects(n("5"))
      completeMock.expects(n("5"))
      completeMock.expects(n("3"))
      visitMock.expects(n("6"))
      visitMock.expects(n("8"))
      completeMock.expects(n("8"))
      visitMock.expects(n("7"))
      completeMock.expects(n("7"))
      completeMock.expects(n("6"))
      completeMock.expects(n("2"))
      completeMock.expects(n("1"))
    }

    graph.dfs.find(startNode = Some(n("1"))).onVisit(node => {
      println(s"visit: $node")
      visitMock(node)
    }).onComplete(node => {
      println(s"completed: $node")
      completeMock(node)
    }).run
  }
}
