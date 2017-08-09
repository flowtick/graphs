package com.flowtick.graphs.layout

import java.io.FileOutputStream

import com.flowtick.graphs.{ Edge, WeightedEdge }
import com.flowtick.graphs.defaults.{ DefaultGraph, DefaultNode, n }
import com.flowtick.graphs.rendering.ShapeDefinition
import com.mxgraph.view.mxGraph
import org.scalatest.FlatSpec

class JGraphXLayoutSpec extends FlatSpec {
  "JGraphX layout" should "layout simple graph and save it" in {
    val graph = DefaultGraph.create { implicit graph =>
      n("A") ~> n("B")
      n("B") ~> n("C")
      n("D") ~> n("A")
    }

    val layoutedGraph = new JGraphXLayout[DefaultNode, Edge[DefaultNode]].layout(graph, _ => None)
    saveLayoutedGraph("simple", layoutedGraph)
  }

  def saveLayoutedGraph(filename: String, layoutedGraph: mxGraph) = {
    JGraphXLayoutRenderer.renderImage(layoutedGraph, new FileOutputStream(s"target/$filename.png"))
    JGraphXLayoutRenderer.renderImage(layoutedGraph, new FileOutputStream(s"target/$filename.svg"), format = "SVG")
  }

  it should "layout city graph" in {
    val cities = DefaultGraph.weighted { implicit g =>
      n("Frankfurt") ~ (85, n("Mannheim"))
      n("Frankfurt") ~ (217, n("Wuerzburg"))
      n("Frankfurt") ~ (173, n("Kassel"))
      n("Mannheim") ~ (80, n("Karlsruhe"))
      n("Wuerzburg") ~ (186, n("Erfurt"))
      n("Wuerzburg") ~ (103, n("Nuernberg"))
      n("Stuttgart") ~ (183, n("Nuernberg"))
      n("Kassel") ~ (502, n("Muenchen"))
      n("Nuernberg") ~ (167, n("Muenchen"))
      n("Karlsruhe") ~ (250, n("Augsburg"))
      n("Augsburg") ~ (84, n("Muenchen"))
    }

    val layoutedGraph = new JGraphXLayout[DefaultNode, WeightedEdge[Int, DefaultNode]].layout(
      cities,
      _ => Some(ShapeDefinition(50, 70, rounded = true, color = "#FF0000", shapeType = "ellipse"))
    )

    saveLayoutedGraph("cities", layoutedGraph)
  }
}
