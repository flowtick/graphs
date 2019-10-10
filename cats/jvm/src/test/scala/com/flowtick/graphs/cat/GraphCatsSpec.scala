package com.flowtick.graphs.cat

import com.flowtick.graphs.defaults._
import cats.implicits._
import com.flowtick.graphs.{ Edge, Graph, Identifiable, NodeContext }
import org.scalatest.{ FlatSpec, Matchers }

class GraphCatsSpec extends FlatSpec with Matchers {
  import com.flowtick.graphs.cat.instances._

  "Graph Monoid" should "combine graphs" in {
    type NumberNodeGraph = Graph[Unit, Int, Unit]

    val graphA: NumberNodeGraph = Graph.fromEdges(Set(
      n(1) --> n(2),
      n(2) --> n(3)))

    val graphB: NumberNodeGraph = Graph.fromEdges(Set(
      n(2) --> n(3),
      n(4) --> n(3),
      n(4) --> n(5),
      n(5) --> n(1)))

    val combined = graphA |+| graphB

    combined.edges should contain theSameElementsAs Seq(
      n(1) --> n(2),
      n(2) --> n(3),
      n(4) --> n(3),
      n(4) --> n(5),
      n(5) --> n(1))

    combined.nodeContext should be(Map(
      5 -> NodeContext(incoming = Set(n(4) --> n(5)), outgoing = Set(n(5) --> n(1))),
      1 -> NodeContext(incoming = Set(n(5) --> n(1)), outgoing = Set(n(1) --> n(2))),
      2 -> NodeContext(incoming = Set(n(1) --> n(2)), outgoing = Set(n(2) --> n(3))),
      3 -> NodeContext(incoming = Set(n(2) --> n(3), n(4) --> n(3)), outgoing = Set.empty[Edge[Unit, Int]]),
      4 -> NodeContext(incoming = Set.empty[Edge[Unit, Int]], outgoing = Set(n(4) --> n(3), n(4) --> n(5)))))
  }

  it should "map over node values" in {
    type WeightedGraph = Graph[Double, Int, Unit]

    val graph = Graph.fromEdges(Set(
      n(1) --> (1.2, n(2)),
      n(2) --> (1.3, n(2))))

    val doubleStringGraph = graphNodeFunctor[Double, Int, Unit].map(graph)(node => (node * 2).toString)

    doubleStringGraph.nodes should be(Set("2", "4"))

    doubleStringGraph.nodeContext.get("2").map(_.outgoing) should be(Some(Set(
      n("2") --> (1.2, n("4")))))

    doubleStringGraph.nodeContext.get("4").map(_.outgoing) should be(Some(Set(
      n("4") --> (1.3, n("4")))))
  }

  it should "contramap identifiable" in {
    case class Foo(fooField: String)
    case class Bar(fooInBar: Foo)

    val fooId: Identifiable[Foo] = Identifiable.identify[Foo](_.fooField)
    val barId: Identifiable[Bar] = fooId.contramap(_.fooInBar)
  }
}
