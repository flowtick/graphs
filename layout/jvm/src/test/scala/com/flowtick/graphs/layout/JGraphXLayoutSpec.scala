package com.flowtick.graphs.layout

import java.io.{ FileOutputStream, OutputStream }

import com.flowtick.graphs.defaults._
import com.flowtick.graphs.defaults.directed._
import com.mxgraph.view.mxGraph
import org.scalatest.FlatSpec

import scala.util.Try

class JGraphXLayoutSpec extends FlatSpec {
  "JGraphX layout" should "layout simple graph and save it" in {
    val graph = defaultGraph.from(Set(
      n("A") --> n("B"),
      n("B") --> n("C"),
      n("D") --> n("A")))

    val layoutedGraph = new JGraphXLayout[DefaultGraph, Edge, Unit, String, Unit].layout(graph, _ => None)
    saveGraph("simple", layoutedGraph)
  }

  def saveGraph(filename: String, layoutedGraph: mxGraph): Try[OutputStream] = {
    JGraphXLayoutRenderer.renderImage(layoutedGraph, new FileOutputStream(s"target/$filename.png"))
    JGraphXLayoutRenderer.renderImage(layoutedGraph, new FileOutputStream(s"target/$filename.svg"), format = "SVG")
  }

  it should "layout city graph" in {
    val cities = defaultGraph.from(Set(
      n("Frankfurt") --> (85, n("Mannheim")),
      n("Frankfurt") --> (217, n("Wuerzburg")),
      n("Frankfurt") --> (173, n("Kassel")),
      n("Mannheim") --> (80, n("Karlsruhe")),
      n("Wuerzburg") --> (186, n("Erfurt")),
      n("Wuerzburg") --> (103, n("Nuernberg")),
      n("Stuttgart") --> (183, n("Nuernberg")),
      n("Kassel") --> (502, n("Muenchen")),
      n("Nuernberg") --> (167, n("Muenchen")),
      n("Karlsruhe") --> (250, n("Augsburg")),
      n("Augsburg") --> (84, n("Muenchen"))))

    val layoutedGraph = new JGraphXLayout[DefaultGraph, Edge, Int, String, Unit].layout(
      cities,
      _ => Some(ShapeDefinition(50, 70, rounded = true, color = "#FF0000", shapeType = "ellipse")))

    saveGraph("cities", layoutedGraph)
  }
}
