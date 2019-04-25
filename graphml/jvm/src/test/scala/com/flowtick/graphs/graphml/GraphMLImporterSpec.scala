package com.flowtick.graphs.graphml

import com.flowtick.graphs.{ Edge, Graph }
import com.flowtick.graphs.layout.JGraphXLayouter
import org.scalatest.{ FlatSpec, Matchers }
import com.flowtick.graphs.defaults._

import scala.xml.Elem

case class FooNode(foo: String)

class GraphMLImporterSpec extends FlatSpec with Matchers {
  /*  "GraphML Importer" should "import rendered XML" in {
    val testEdge: Edge[Unit, GraphMLNode[FooNode]] =
      n(graphMlNode("A", FooNode("bar"))) --> n(graphMlNode("B", FooNode("baz")))

    val testGraph: Graph[Unit, GraphMLNode[FooNode], Unit] = Graph.from(Seq(
      testEdge))
    val xml: Elem = new GraphMLRenderer().render(testGraph, JGraphXLayouter)

    val imported: Either[Throwable, Graph[GraphMLEdge[Unit], GraphMLNode[Option[FooNode]], GraphMLGraph]] =
      GraphMLImporter.fromXml[Unit, Option[FooNode]](xml.toString)

    imported.right.foreach { graphml =>
      val importedNodes = graphml.nodes.toList.sortBy(_.id)
      importedNodes should have size 2

      importedNodes.headOption match {
        case Some(aNode) =>
          aNode.id should be("A")
          aNode.properties.find(_.key.id == "foo") should be(Some(GraphMLProperty(GraphMLKey("foo", None, Some("string"), Some("node"), None), "bar")))
          aNode.properties.find(_.key.id == "graphics") should be(defined)
        case _ => fail()
      }

      importedNodes(1) match {
        case bNode =>
          bNode.id should be("B")
          bNode.properties.find(_.key.id == "graphics") should be(defined)
      }

      val importedEdges = graphml.edges
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
    val imported = GraphMLImporter.fromXml[Double, String](cities.getLines().mkString)
    imported.right.toOption match {
      case Some(graphml) =>
        graphml.nodes.find(_.id == "n0") match {
          case Some(n0) =>
            n0.id should be("n0")
            n0.label should be(Some("Karlsruhe"))
          case _ => fail("unable to find node n0")
        }

        graphml.edges.map(_.value).find(_.id == "e1") match {
          case Some(e1) =>
            e1.properties.find(_.key.id == "d7") should be(Some(GraphMLProperty(GraphMLKey("d7", Some("Property 1"), Some("string"), Some("edge"), None), "test")))
            e1.label should be(Some("42"))
          case _ => fail("unable to find edge e1")
        }
      case _ => fail("no graph imported")
    }
  }*/
}