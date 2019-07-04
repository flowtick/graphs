package com.flowtick.graphs.graphml

import cats.data.NonEmptyList
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
      case None => fail
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
          aNode.properties.find(_.key == "graphics") should be(empty)
        case _ => fail()
      }

      importedNodes(1) match {
        case bNode =>
          bNode.id should be("2")
          bNode.value should be(TestNode("C", "D"))
          bNode.properties.find(_.key == "graphics") should be(empty)
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
    val imported = fromGraphML[String, String, Unit](cities.getLines().mkString)

    imported match {
      case Right(graphml) =>
        graphml.nodes.find(_.id == "n0") match {
          case Some(n0) =>
            n0.id should be("n0")
            n0.label should be(Some("Kassel"))
          case _ => fail("unable to find node n0")
        }

        graphml.edges.map(_.value).find(_.id == "e1") match {
          case Some(e1) =>
            e1.value should equal("85")
            e1.label should be(empty)
          case _ => fail("unable to find edge e1")
        }
      case Left(errors) => fail(s"error during parsing: $errors")
    }
  }

  it should "serialize the cities graph" in {
    import com.flowtick.graphs.defaults._

    val cities: Graph[Int, String, Unit] = Graph.from(Set(
      n("Frankfurt") --> (85, n("Mannheim")),
      n("Frankfurt") --> (217, n("Wuerzburg")),
      n("Frankfurt") --> (173, n("Kassel")),
      n("Mannheim") --> (80, n("Karlsruhe")),
      n("Wuerzburg") --> (186, n("Erfurt")),
      n("Wuerzburg") --> (103, n("Nuernberg")),
      n("Stuttgart") --> (183, n("Nuernberg")),
      n("Kassel") --> (502, n("Muenchen")),
      n("Nuernberg") --> (167, n("Muenchen")),
      n("Karlsruhe") --> (250, n("Augsburg")),
      n("Augsburg") --> (84, n("Muenchen"))))

    val graphML = cities.toGraphML()
    val xml = graphML.xml

    xml.headOption.foreach(println)

    val parsed: Either[NonEmptyList[Throwable], GraphMLGraphType[Int, String, Unit]] = fromGraphML[Int, String, Unit](xml.toString)

    parsed match {
      case Right(parsedGraph) =>
        parsedGraph.nodes.find(_.id == "Kassel") should be(graphML.nodes.find(_.id == "Kassel"))
        parsedGraph.nodeContext.find(_._1.id == "Kassel") should be(graphML.nodeContext.find(_._1.id == "Kassel"))

        parsedGraph.nodes should contain theSameElementsAs graphML.nodes
        parsedGraph.nodeContext.values should contain theSameElementsAs graphML.nodeContext.values

      case Left(errors) => fail(s"parsing errors $errors")
    }
  }

}
