package com.flowtick.graphs.layout

import java.io.{ FileOutputStream, OutputStream }

import com.flowtick.graphs.Graph
import com.flowtick.graphs.defaults._
import com.flowtick.graphs.defaults.label._
import com.mxgraph.view.mxGraph
import org.scalatest.FlatSpec

import scala.util.Try

class JGraphXLayoutSpec extends FlatSpec {
  "JGraphX layout" should "layout simple graph and save it" in {
    val graph = Graph.fromEdges(Set(
      "A" --> "B",
      "B" --> "C",
      "D" --> "A"))

    val layoutedGraph = new JGraphXLayout[Unit, String, Unit].layout(graph)
    saveGraph("simple", layoutedGraph)
  }

  def saveGraph(filename: String, layoutedGraph: mxGraph): Try[OutputStream] = {
    JGraphXLayoutRenderer.renderImage(layoutedGraph, new FileOutputStream(s"target/$filename.png"))
    JGraphXLayoutRenderer.renderImage(layoutedGraph, new FileOutputStream(s"target/$filename.svg"), format = "SVG")
  }

  it should "layout city graph" in {
    val cities = Graph.fromEdges(Set(
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

    val layoutedGraph = new JGraphXLayout[Int, String, Unit].layout(cities)

    saveGraph("cities", layoutedGraph)
  }
}
