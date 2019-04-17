package com.flowtick.graphs.cat

import com.flowtick.graphs.defaults._
import cats.implicits._
import com.flowtick.graphs.Identifiable
import org.scalatest.{ FlatSpec, Matchers }

class GraphCatsSpec extends FlatSpec with Matchers {
  import com.flowtick.graphs.cat.instances._

  "Graph Monoid" should "combine graphs" in {
    type NumberNodeGraph = DefaultGraph[Unit, Int, Unit]

    val graphA: NumberNodeGraph = Graph.from(Set(
      n(1) --> n(2),
      n(2) --> n(3)))

    val graphB: NumberNodeGraph = Graph.from(Set(
      n(2) --> n(3),
      n(4) --> n(3),
      n(4) --> n(5)))

    val combined = graphA |+| graphB

    defaultGraph.edges(combined) should contain theSameElementsAs Seq(
      n(1) --> n(2),
      n(2) --> n(3),
      n(4) --> n(3),
      n(4) --> n(5))
  }

  it should "map over node values" in {
    type WeightedGraph = DefaultGraph[Double, Int, Unit]

    val graph = Graph.from(Set(
      n(1) --> (1.2, n(2)),
      n(2) --> (1.3, n(2))))

    val doubleStringGraph = graphNodeFunctor[DefaultGraph, Double, Int, Unit].map(graph)(node => (node * 2).toString)

    defaultGraph.nodes(doubleStringGraph) should be(Set("2", "4"))

    defaultGraph.outgoing(doubleStringGraph).get("2").map(_.toList) should be(Some(List(
      n("2") --> (1.2, n("4")))))
    defaultGraph.outgoing(doubleStringGraph).get("4").map(_.toList) should be(Some(List(
      n("4") --> (1.3, n("4")))))
  }

  it should "contramap identifiable" in {
    case class Foo(fooField: String)
    case class Bar(fooInBar: Foo)

    val fooId: Identifiable[Foo] = Identifiable.identify[Foo](_.fooField)
    val barId: Identifiable[Bar] = fooId.contramap(_.fooInBar)
  }
}
