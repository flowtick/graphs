package com.flowtick.graphs.layout

import java.io.{ FileOutputStream, OutputStream }

import com.flowtick.graphs.defaults.{ DefaultEdge, DefaultGraph, DefaultNode, WeightedEdge, n }
import com.flowtick.graphs.defaults.directed._
import com.mxgraph.view.mxGraph
import org.scalatest.FlatSpec

import scala.util.Try

class JGraphXLayoutSpec extends FlatSpec {
  "JGraphX layout" should "layout simple graph and save it" in {
    val graph = DefaultGraph.create(Seq(
      n("A") -> n("B"),
      n("B") -> n("C"),
      n("D") -> n("A")))

    val layoutedGraph = new JGraphXLayout[DefaultNode, DefaultEdge[DefaultNode]].layout(graph, _ => None)
    saveGraph("simple", layoutedGraph)
  }

  def saveGraph(filename: String, layoutedGraph: mxGraph): Try[OutputStream] = {
    JGraphXLayoutRenderer.renderImage(layoutedGraph, new FileOutputStream(s"target/$filename.png"))
    JGraphXLayoutRenderer.renderImage(layoutedGraph, new FileOutputStream(s"target/$filename.svg"), format = "SVG")
  }

  it should "layout city graph" in {
    val cities = DefaultGraph.weighted[DefaultEdge[DefaultNode], DefaultNode, Int](Seq.empty)

    val layoutedGraph = new JGraphXLayout[DefaultNode, WeightedEdge[DefaultEdge[DefaultNode], Int]].layout(
      cities,
      _ => Some(ShapeDefinition(50, 70, rounded = true, color = "#FF0000", shapeType = "ellipse")))

    saveGraph("cities", layoutedGraph)
  }
}
