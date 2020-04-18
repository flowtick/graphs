package com.flowtick.graphs.graphml

import cats.data.NonEmptyList
import cats.data.Validated.Valid
import com.flowtick.graphs.Graph
import com.flowtick.graphs.graphml.generic._
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable

case class Foo(bar: String, baz: Double)
case class TestNode(first: String, second: String)

class GraphMLDatatypeSpec extends FlatSpec with Matchers {
  implicit val labelledGenericTestNode = shapeless.LabelledGeneric[TestNode]
  implicit val labelledGenericFoo = shapeless.LabelledGeneric[Foo]

  val testGraph: Graph[GraphMLGraph[Unit], GraphMLEdge[Unit], GraphMLNode[TestNode]] = GraphML(
    id = "new-graph",
    meta = (),
    edges = Set(ml(TestNode("A", "B"), Some("1")) --> ml(TestNode("C", "D"), Some("2"))))

  val testDataType = new GraphMLDatatype[Unit, Unit, TestNode]()

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
    import Datatype._

    val fooDataType: Datatype[Foo] = Datatype[Foo]

    val fooXml = fooDataType.serialize(Foo("bar", 42.0))

    fooXml.toString should be(<value>bar</value><value>42.0</value>.mkString(""))

    val deserialized = fooDataType.deserialize(fooXml, Map.empty)

    deserialized should be(Valid(Foo("bar", 42.0)))
  }

  it should "deserialize rendered XML" in {
    val imported = FromGraphML[Unit, Unit, TestNode](testDataType.serialize(testGraph).mkString(""))

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
          edge.from.id should be("1")
          edge.to.id should be("2")
        case None => fail
      }
    }
  }

  it should "import xml created by yed with node and edge properties" in {
    val cities = io.Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("yed-cities.graphml"))
    val imported = FromGraphML[Unit, String, String](cities.getLines().mkString)

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
      case Left(errors) => fail(s"error during parsing: ${errors.toString}")
    }
  }

  it should "serialize the cities graph" in {
    import com.flowtick.graphs.defaults._
    import com.flowtick.graphs.defaults.label._

    val cities: Graph[Unit, Int, String] = Graph.fromEdges(Set(
      "Frankfurt" --> (85, "Mannheim"),
      "Frankfurt" --> (217, "Wuerzburg"),
      "Frankfurt" --> (173, "Kassel"),
      "Mannheim" --> (80, "Karlsruhe"),
      "Wuerzburg" --> (186, "Erfurt"),
      "Wuerzburg" --> (103, "Nuernberg"),
      "Stuttgart" --> (183, "Nuernberg"),
      "Kassel" --> (502, "Muenchen"),
      "Nuernberg" --> (167, "Muenchen"),
      "Karlsruhe" --> (250, "Augsburg"),
      "Augsburg" --> (84, "Muenchen")))

    val converter = new GraphMLConverterOps(cities)

    val graphML = converter.asGraphML
    val xml = graphML.xml

    xml.headOption.foreach(println)

    val parsed: Either[NonEmptyList[Throwable], GraphMLGraphType[Unit, Int, String]] = FromGraphML[Unit, Int, String](xml.toString)

    parsed match {
      case Right(parsedGraph) =>
        parsedGraph.nodes.find(_.id == "Kassel") should be(graphML.nodes.find(_.id == "Kassel"))
        parsedGraph.nodes should contain theSameElementsAs graphML.nodes
        parsedGraph.edges should contain theSameElementsAs graphML.edges

      case Left(errors) => fail(s"parsing errors ${errors.toString}")
    }
  }

}
