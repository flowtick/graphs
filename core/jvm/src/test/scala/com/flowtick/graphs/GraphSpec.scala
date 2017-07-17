package com.flowtick.graphs

import com.flowtick.graphs.defaults._
import org.scalatest.{ FlatSpec, Matchers }

class GraphSpec extends FlatSpec with Matchers {

  val graph = Graph.create[DefaultNode, Edge[DefaultNode]] { implicit g =>
    n("A") ~> n("B")
    n("B") ~> n("C")
    n("C") ~> n("D")
    n("D") ~> n("A")
    n("A") ~> n("C")
    n("B") ~> n("D")
  }

  "Graph" should "provide incoming edges for nodes" in {
    graph.incoming(n("A")).toList should be(List(
      DefaultDirectedEdge(None, n("D"), n("A"))
    ))

    graph.incoming(n("B")).toList should be(List(
      DefaultDirectedEdge(None, n("A"), n("B"))
    ))

    graph.incoming(n("C")).toList should be(List(
      DefaultDirectedEdge(None, n("B"), n("C")),
      DefaultDirectedEdge(None, n("A"), n("C"))
    ))

    graph.incoming(n("D")).toList should be(List(
      DefaultDirectedEdge(None, n("C"), n("D")),
      DefaultDirectedEdge(None, n("B"), n("D"))
    ))
  }

  "Graph" should "provide outgoing edges for nodes" in {
    graph.outgoing(n("A")).toList should be(List(
      DefaultDirectedEdge(None, n("A"), n("C")),
      DefaultDirectedEdge(None, n("A"), n("B"))
    ))

    graph.outgoing(n("B")).toList should be(List(
      DefaultDirectedEdge(None, n("B"), n("C")),
      DefaultDirectedEdge(None, n("B"), n("D"))
    ))

    graph.outgoing(n("C")).toList should be(List(
      DefaultDirectedEdge(None, n("C"), n("D"))
    ))

    graph.outgoing(n("D")).toList should be(List(
      DefaultDirectedEdge(None, n("D"), n("A"))
    ))
  }

  "Graph" should "provide incoming and outgoing edges for undirected edges" in {
    val graph = Graph.create[DefaultNode, Edge[DefaultNode]] { implicit g =>
      n("A") ~ n("B")
      n("C") ~> n("B")
      n("D") ~> n("A")
    }

    graph.incoming(n("A")).toList should be(List[Edge[DefaultNode]](
      DefaultDirectedEdge(None, n("D"), n("A")),
      DefaultUndirectedEdge(None, n("A"), n("B"))
    ))

    graph.outgoing(n("A")).toList should be(List(
      DefaultUndirectedEdge(None, n("A"), n("B"))
    ))

    graph.incoming(n("B")).toList should be(List[Edge[DefaultNode]](
      DefaultDirectedEdge(None, n("C"), n("B")),
      DefaultUndirectedEdge(None, n("A"), n("B"))
    ))
  }

}
