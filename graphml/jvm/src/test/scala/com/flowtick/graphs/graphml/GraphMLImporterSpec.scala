package com.flowtick.graphs.graphml

import com.flowtick.graphs.Edge
import com.flowtick.graphs.layout.JGraphXLayouter
import org.scalatest.{ FlatSpec, Matchers }
import com.flowtick.graphs.defaults._
import com.flowtick.graphs.defaults.directed._

import scala.xml.Elem

case class FooNode(foo: String)

class GraphMLImporterSpec extends FlatSpec with Matchers {
  "GraphML Importer" should "import rendered XML" in {
    val testEdge: Edge[Unit, GraphMLNode[Option[FooNode]]] =
      n(graphMlNode("A", Option(FooNode("bar")))) --> n(graphMlNode("B", None))

    val testGraph = directedGraph.from(Seq(
      testEdge))
    val xml: Elem = new GraphMLRenderer().render(testGraph, JGraphXLayouter)

    val imported: Either[Throwable, DefaultGraph[GraphMLEdge[Unit], GraphMLNode[Option[FooNode]], GraphMLGraph]] =
      GraphMLImporter.fromXml[DefaultGraph, Unit, Option[FooNode]](xml.toString)

    imported.right.foreach { graphml =>
      val importedNodes = directedGraph.nodes(graphml).toList.sortBy(_.id)
      importedNodes should have size 2

      importedNodes.headOption match {
        case Some(aNode) =>
          aNode.id should be("A")
          aNode.properties.get("foo") should be(Some(GraphMLProperty(GraphMLKey("foo", None, Some("string"), Some("node"), None), "bar")))
          aNode.properties.get("graphics") should be(defined)
        case _ => fail()
      }

      importedNodes(1) match {
        case bNode =>
          bNode.id should be("B")
          bNode.properties.get("graphics") should be(defined)
      }

      val importedEdges = directedGraph.edges(graphml)
      importedEdges should have size 1
      importedEdges.headOption match {
        case Some(edge) =>
          edge.value.id should be("A-B")
          edge.head.id should be("A")
          edge.tail.id should be("B")
        case None => fail
      }
    }
  }

  it should "import xml created by yed with node and edge properties" in {
    val cities = io.Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("yed-cities.graphml"))
    val imported = GraphMLImporter.fromXml[DefaultGraph, Double, String](cities.getLines().mkString)
    imported.right.toOption match {
      case Some(graphml) =>
        directedGraph.nodes(graphml).find(_.id == "n0") match {
          case Some(n0) =>
            n0.id should be("n0")
            n0.label should be(Some("Karlsruhe"))
          case _ => fail("unable to find node n0")
        }

        directedGraph.edges(graphml).map(_.value).find(_.id == "e1") match {
          case Some(e1) =>
            e1.properties.get("d7") should be(Some(GraphMLProperty(GraphMLKey("d7", Some("Property 1"), Some("string"), Some("edge"), None), "test")))
            e1.label should be(Some("42"))
          case _ => fail("unable to find edge e1")
        }
      case _ => fail("no graph imported")
    }
  }
}