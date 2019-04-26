package com.flowtick.graphs

import com.flowtick.graphs.defaults._
import com.flowtick.graphs.graphml.{ GraphMLImporter, GraphMLDatatype }
import com.flowtick.graphs.layout.GraphLayout
import org.scalatest.{ FlatSpec, Matchers }

import scala.xml.Elem

class JsGraphMLImportSpec extends FlatSpec with Matchers {
  "GraphRenderer" should "import graph from XML" in new GraphMLDatatype {
    val graph = Graph.from(Seq(
      n("A") --> n("B")))

    val xml: Elem = render(graph, GraphLayout.none)

    GraphMLImporter.fromXml[Unit, String](xml.toString).isRight should be(true)
  }
}
