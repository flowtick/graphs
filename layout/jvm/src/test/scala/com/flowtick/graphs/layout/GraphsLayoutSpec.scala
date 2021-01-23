package com.flowtick.graphs.layout

import com.flowtick.graphs.Graph
import com.flowtick.graphs.defaults._
import com.flowtick.graphs.defaults.label._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GraphsLayoutSpec extends AnyFlatSpec with Matchers {
  "Graph layout" should "layout simple graph" in {
    val graph = Graph.fromEdges[Unit, String](Set(
      "A" --> "B",
      "B" --> "C",
      "D" --> "A"))
    println(ELkLayout.layout(graph))
  }

  it should "layout city graph" in {
    val cities = Graph.fromEdges[Int, String](Set(
      "Frankfurt" --> (85, "Mannheim"),
      "Frankfurt" --> (217, "Wuerzburg"),
      "Frankfurt" --> (173, "Kassel"),
      "Mannheim" --> (80, "Karlsruhe"),
      "Wuerzburg" --> (186, "Erfurt"),
      "Wuerzburg" --> (103, "Nuernberg"),
      "Stuttgart" --> (183, "Nuernberg"),
      "Kassel" --> (502, "Muenchen"),
      "Nuernberg" --> (167, "Muenchen"),
      "Karlsruhe" --> (250, "Augsburg"),
      "Augsburg" --> (84, "Muenchen")))
    println(ELkLayout.layout(cities))
  }
}
