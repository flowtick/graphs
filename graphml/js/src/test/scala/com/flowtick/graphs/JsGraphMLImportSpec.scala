package com.flowtick.graphs

import com.flowtick.graphs.defaults._
import com.flowtick.graphs.defaults.directed._
import com.flowtick.graphs.graphml.{ GraphMLImporter, GraphMLRenderer }
import com.flowtick.graphs.layout.GraphLayout
import org.scalatest.{ FlatSpec, Matchers }

import scala.xml.Elem

class JsGraphMLImportSpec extends FlatSpec with Matchers {
  "GraphRenderer" should "import graph from XML" in new GraphMLRenderer {
    val graph = defaultGraph.from(Seq(
      n("A") --> n("B")))

    val xml: Elem = render(graph, GraphLayout.none)

    new GraphMLImporter[DefaultGraph, Edge]().fromXml(xml.toString).isRight should be(true)
  }
}
