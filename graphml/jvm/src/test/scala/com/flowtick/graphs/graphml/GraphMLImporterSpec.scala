package com.flowtick.graphs.graphml

import org.scalatest.{ FlatSpec, Matchers }

class GraphMLImporterSpec extends FlatSpec with Matchers {
  "GraphML Importer" should "import rendered XML" in {
    val graph = GraphMLGraph.create("test-graph") { implicit graph =>
      node("A", nodeProperty("foo", "bar", typeHint = Some("string"))) ~> node("B")
    }

    val graphml = new GraphMLRenderer().render(graph)
    val imported = new GraphMLImporter().fromXml(graphml.toString)

    imported.right.foreach { graph =>
      val importedNodes = graph.nodes.toList.sortBy(_.id)
      importedNodes should have size 2

      importedNodes.headOption match {
        case Some(aNode) =>
          aNode.id should be("A")
          aNode.properties should contain("foo" -> GraphMLProperty(GraphMLKey("foo", None, Some("string"), Some("node"), None), "bar"))
          aNode.properties.get("graphics") should be(defined)
        case _ => fail()
      }

      importedNodes(1) match {
        case bNode =>
          bNode.id should be("B")
          bNode.properties.get("graphics") should be(defined)
      }

      val importedEdges = graph.edges
      importedEdges should have size 1
      importedEdges.headOption.foreach(_.id should be("A-B"))
      importedEdges.headOption.foreach(_.source.id should be("A"))
      importedEdges.headOption.foreach(_.target.id should be("B"))
    }
  }

  it should "import xml exported with yed and edge properties" in {
    val cities = io.Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("yed-cities.graphml"))
    val imported = new GraphMLImporter().fromXml(cities.getLines().mkString)
    imported.right.toOption match {
      case Some(graph) =>
        graph.edges.find(_.id == "e1") match {
          case Some(e1) =>
            e1.properties should contain("d7" -> GraphMLProperty(GraphMLKey("d7", Some("Property 1"), Some("string"), Some("edge"), None), "test"))
            e1.label should be(Some("42"))
          case _ => fail()
        }
      case _ => fail()
    }
  }
}