package com.flowtick.graphs.graphml

import org.scalatest.{ FlatSpec, Matchers }
import com.flowtick.graphs.defaults._

class GraphMLImporterSpec extends FlatSpec with Matchers {
  "GraphML Importer" should "import XML" in {
    val graph = DefaultGraph.create { implicit graph =>
      n("A") ~> n("B")
    }

    val graphml = new GraphMLRenderer().render(graph)
    val imported = new GraphMLImporter().fromXml(graphml.toString)

    imported.right.map(_.nodes).getOrElse(Seq.empty) should contain only (
      GraphMLNode("A"),
      GraphMLNode("B")
    )

    imported.right.map(_.edges).getOrElse(Seq.empty) should contain only GraphMLEdge("A-B", None, GraphMLNode("A"), GraphMLNode("B"))
  }
}