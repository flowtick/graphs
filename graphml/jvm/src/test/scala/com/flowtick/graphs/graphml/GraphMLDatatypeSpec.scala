package com.flowtick.graphs.graphml

import cats.data.Validated.Valid
import com.flowtick.graphs.Graph
import org.scalatest.{ FlatSpec, Matchers }
import shapeless._
import shapeless.Typeable
import shapeless.Typeable._
import shapeless.ops.traversable.FromTraversable
import shapeless.ops.traversable.FromTraversable._
import shapeless.ops.traversable.FromTraversable._

case class TestNode(first: String, second: String)

class GraphMLDatatypeSpec extends FlatSpec with Matchers {
  implicit val labelledGenericTestNode = shapeless.LabelledGeneric[TestNode]

  val testGraph: Graph[GraphMLEdge[Unit], GraphMLNode[TestNode], GraphMLGraph[Unit]] = GraphMLGraph(
    id = "new-graph",
    meta = (),
    edges = Set(ml(TestNode("A", "B"), Some("1")) --> ml(TestNode("C", "D"), Some("2"))))

  val testDataType = new GraphMLDatatype[Unit, TestNode, Unit]()

  def prettyPrint(xml: scala.xml.Node) = println(new scala.xml.PrettyPrinter(80, 4).format(xml))

  "GraphRenderer" should "render simple graph" in {
    testDataType.serialize(testGraph).headOption match {
      case Some(xml) =>
        println(xml)
        prettyPrint(xml)

        xml.headOption shouldBe defined
        xml.headOption.foreach(_.label should be("graphml"))
        xml.descendant.count(_.label == "node") should be(2)
        xml.descendant.count(_.label == "edge") should be(1)

        xml.descendant.filter(_.label == "node").flatMap(_.attribute("id").map(_.text)) should contain theSameElementsAs List("1", "2")
        xml.descendant.filter(_.label == "edge").flatMap(_.attribute("id").map(_.text)) should be(List("1-2"))
    }
  }

  /*it should "render weighted graph" in {
    val weigthed: Graph[GraphMLEdge[Double], GraphMLNode[TestNode], GraphMLGraph[Unit]] = GraphMLGraph("weighted", (), Set(ml(TestNode("A", "B"), id = Some("1")) --> (42.0, ml(TestNode("C", "D"), id = Some("2")))))

    new GraphMLDatatype[Unit, TestNode, Unit]().serialize(weigthed).headOption match {
      case Some(xml) => prettyPrint(xml)
    }
  }*/

  it should "serialize a product" in {
    case class Foo(bar: String, baz: Double)

    val fooDataType = implicitly[Datatype[Foo]]

    val fooXml = fooDataType.serialize(Foo("bar", 42.0))

    fooXml.toString should be(<value>bar</value><value>42.0</value>.mkString(""))

    val deserialized = fooDataType.deserialize(fooXml)

    deserialized should be(Valid(Foo("bar", 42.0)))
  }

  it should "deserialize rendered XML" in {
    val imported = testDataType.fromXml(testDataType.serialize(testGraph).mkString(""))

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
    val imported = testDataType.fromXml(cities.getLines().mkString)
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
  }

}
