package com.flowtick.graphs.graphml

import org.scalatest.{ FlatSpec, Matchers }

class GraphMLImporterSpec extends FlatSpec with Matchers {
  "GraphML Importer" should "import XML" in {
    val graph = GraphMLGraph.create("test-graph") { implicit graph =>
      node("A", properties = Map("foo" -> "bar")) ~> node("B")
    }

    val graphml = new GraphMLRenderer().render(graph)
    val imported = new GraphMLImporter().fromXml(graphml.toString)

    val importedNodes = imported.right.map(_.nodes).getOrElse(Seq.empty).toList
    importedNodes should have size 2
    importedNodes.headOption.foreach(_.id should be("A"))
    importedNodes.headOption.foreach(_.properties should contain("foo" -> "bar"))
    importedNodes.headOption.foreach(_.properties.get("graphics") should be(defined))
    importedNodes(1).id should be("B")
    importedNodes(1).properties.get("graphics") should be(defined)

    val importedEdges = imported.right.map(_.edges).getOrElse(Seq.empty).toList
    importedEdges should have size 1
    importedEdges.headOption.foreach(_.id should be("A-B"))
    importedEdges.headOption.foreach(_.source.id should be("A"))
    importedEdges.headOption.foreach(_.target.id should be("B"))
  }
}