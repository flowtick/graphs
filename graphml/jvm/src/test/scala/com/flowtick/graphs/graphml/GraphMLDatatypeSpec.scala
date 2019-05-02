package com.flowtick.graphs.graphml

import cats.data.Validated.Valid
import com.flowtick.graphs.Graph
import org.scalatest.{ FlatSpec, Matchers }

import scala.collection.immutable

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
        prettyPrint(xml)

        xml.headOption shouldBe defined
        xml.headOption.foreach(_.label should be("graphml"))
        xml.descendant.count(_.label == "node") should be(2)
        xml.descendant.count(_.label == "edge") should be(1)

        xml.descendant.filter(_.label == "node").flatMap(_.attribute("id").map(_.text)) should contain theSameElementsAs List("1", "2")
        xml.descendant.filter(_.label == "edge").flatMap(_.attribute("id").map(_.text)) should be(List("1-2"))
    }
  }

  it should "serialize a product" in {
    case class Foo(bar: String, baz: Double)

    val fooDataType = implicitly[Datatype[Foo]]

    val fooXml = fooDataType.serialize(Foo("bar", 42.0))

    fooXml.toString should be(<value>bar</value><value>42.0</value>.mkString(""))

    val deserialized = fooDataType.deserialize(fooXml, Map.empty)

    deserialized should be(Valid(Foo("bar", 42.0)))
  }

  it should "deserialize rendered XML" in {
    val imported = fromGraphML[Unit, TestNode, Unit](testDataType.serialize(testGraph).mkString(""))

    imported.right.foreach { graphml =>
      val importedNodes: immutable.Seq[GraphMLNode[TestNode]] = graphml.nodes.toList.sortBy(_.id)
      importedNodes should have size 2

      importedNodes.headOption match {
        case Some(aNode) =>
          aNode.id should be("1")
          aNode.value should be(TestNode("A", "B"))
          aNode.properties.find(_.key == "graphics") should be(defined)
        case _ => fail()
      }

      importedNodes(1) match {
        case bNode =>
          bNode.id should be("2")
          bNode.value should be(TestNode("C", "D"))
          bNode.properties.find(_.key == "graphics") should be(defined)
      }

      val importedEdges = graphml.edges
      importedEdges should have size 1
      importedEdges.headOption match {
        case Some(edge) =>
          edge.value.id should be("1-2")
          edge.head.id should be("1")
          edge.tail.id should be("2")
        case None => fail
      }
    }
  }

  it should "import xml created by yed with node and edge properties" in {
    val cities = io.Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("yed-cities.graphml"))
    val imported = fromGraphML[Unit, Unit, Unit](cities.getLines().mkString)

    imported match {
      case Right(graphml) =>
        graphml.nodes.find(_.id == "n0") match {
          case Some(n0) =>
            n0.id should be("n0")
            n0.label should be(Some("Karlsruhe"))
          case _ => fail("unable to find node n0")
        }

        graphml.edges.map(_.value).find(_.id == "e1") match {
          case Some(e1) =>
            e1.properties.find(_.key == "d7") should be(Some(GraphMLProperty("d7", "test")))
            e1.label should be(Some("42"))
          case _ => fail("unable to find edge e1")
        }
      case Left(errors) => fail(s"error during parsing: $errors")
    }
  }

}
