package com.flowtick.graphs.cat

import com.flowtick.graphs.defaults._
import com.flowtick.graphs.defaults.directed._
import cats.implicits._

import org.scalatest.{ FlatSpec, Matchers }

class GraphMonoidSpec extends FlatSpec with Matchers {
  "Graph Monoid" should "combine graphs" in {
    val graphA = defaultGraph.from(Set(
      n(1) --> n(2),
      n(2) --> n(3)))

    val graphB = defaultGraph.from(Set(
      n(2) --> n(3),
      n(4) --> n(3),
      n(4) --> n(5)))

    val combined = graphMonoid[DefaultGraph, Edge, Unit, Int, Unit].combine(graphA, graphB)

    defaultGraph.edges(combined) should contain theSameElementsAs Seq(
      n(1) --> n(2),
      n(2) --> n(3),
      n(4) --> n(3),
      n(4) --> n(5))
  }
}
