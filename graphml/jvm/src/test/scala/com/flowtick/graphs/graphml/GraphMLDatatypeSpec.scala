package com.flowtick.graphs.graphml

import cats.data.NonEmptyList
import cats.data.Validated.Valid
import com.flowtick.graphs.{Graph, Node}
import com.flowtick.graphs.graphml.generic._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable

case class Foo(bar: String, baz: Double)
case class TestNode(first: String, second: String)

class GraphMLDatatypeSpec extends AnyFlatSpec with Matchers {
  implicit val labelledGenericTestNode = shapeless.LabelledGeneric[TestNode]
  implicit val labelledGenericFoo = shapeless.LabelledGeneric[Foo]

  val testGraph = GraphML.fromEdges(
    edges = Set(ml(TestNode("A", "B"), Some("1")) --> ml(TestNode("C", "D"), Some("2"))))

  val testDataType = GraphMLDatatype[Unit, TestNode]

  def prettyPrint(xml: scala.xml.Node) = println(new scala.xml.PrettyPrinter(80, 4).format(xml))

  "GraphRenderer" should "render simple graph" in {
    testDataType.serialize(testGraph, None).headOption match {
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
    val fooDataType: Datatype[Foo] = Datatype[Foo]

    val fooXml = fooDataType.serialize(Foo("bar", 42.0), Some("node"))

    fooXml.toString should be(<data key="node_bar" type="string">bar</data><data key="node_baz" type="double">42.0</data>.mkString(""))

    val deserialized = fooDataType.deserialize(fooXml, Map.empty, None)

    deserialized should be(Valid(Foo("bar", 42.0)))
  }

  it should "deserialize rendered XML" in {
    val imported = FromGraphML[Unit, TestNode](testDataType.serialize(testGraph, None).mkString(""))

    imported.right.foreach { graphml =>
      val importedNodes: immutable.Seq[Node[GraphMLNode[TestNode]]] = graphml.graph.nodes.toList.sortBy(_.id)
      importedNodes should have size 2

      importedNodes.headOption match {
        case Some(aNode) =>
          aNode.id should be("1")
          aNode.value.value should be(TestNode("A", "B"))
        case _ => fail()
      }

      importedNodes(1) match {
        case bNode =>
          bNode.id should be("2")
          bNode.value.value should be(TestNode("C", "D"))
      }

      val importedEdges = graphml.graph.edges
      importedEdges should have size 1
      importedEdges.headOption match {
        case Some(edge) =>
          edge.value.id should be("1-2")
          edge.from should be("1")
          edge.to should be("2")
        case None => fail
      }
    }
  }

  it should "import xml created by yed with node and edge properties" in {
    val cities = io.Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("yed-cities.graphml"))
    val imported = FromGraphML[String, String](cities.getLines().mkString)

    imported match {
      case Right(graphml) =>
        graphml.graph.nodes.find(_.id == "n0") match {
          case Some(n0) =>
            n0.id should be("n0")
            n0.value.shape.flatMap(_.label.map(_.text)) should be(Some("Kassel"))
          case _ => fail("unable to find node n0")
        }

        graphml.graph.edges.map(_.value).find(_.id == "e1") match {
          case Some(e1) =>
            e1.value should equal("85")
            e1.shape.flatMap(_.label.map(_.text)) should be(empty)
          case _ => fail("unable to find edge e1")
        }
      case Left(errors) => fail(s"error during parsing: ${errors.toString}")
    }
  }

  it should "import graphml test graph with resources" in {
    val testGraph = io.Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("test.graphml"))
    val imported = FromGraphML[Option[String], Option[String]](testGraph.getLines().mkString)

    imported match {
      case Right(graphml) =>
        graphml.meta.resources should have size(1)
        val firstResource = graphml.meta.resources.headOption
        firstResource.map(_.id) should be(Some("1"))
        val value = firstResource.map(_.value).getOrElse(fail("value missing"))
        value should startWith("&lt;?xml version=\"1.0\"")

      case Left(errors) => fail(s"error during parsing: ${errors.toString}")
    }
  }

  it should "serialize the cities graph" in {
    import com.flowtick.graphs.defaults._
    import com.flowtick.graphs.defaults.label._

    val cities: Graph[Int, String] = Graph.fromEdges(Set(
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

    val graphML = converter.asGraphML(Some({
      case (labelString, _) => NodeShape(label = {
        labelString.map(NodeLabel(_, textColor = Some("#000000"), fontSize = Some("12"), fontFamily = Some("Dialog"), modelName = Some("custom"), position = Some(PointSpec(0.0, 0.0))))
      })
    }))

    val xml = graphML.xml

    xml.headOption.foreach(println)

    val parsed: Either[NonEmptyList[Throwable], GraphMLGraph[Int, String]] = FromGraphML[Int, String](xml.toString)

    parsed match {
      case Right(parsedGraph) =>
        val expectedCity = graphML.graph
          .nodes
          .find(_.id == "Kassel")

        val actualCity = parsedGraph.graph.nodes.find(_.id == "Kassel")

        actualCity should be(expectedCity)
        parsedGraph.graph.nodes.map(_.id) should contain theSameElementsAs graphML.graph.nodes.map(_.id)
        parsedGraph.graph.edges.map(_.id) should contain theSameElementsAs graphML.graph.edges.map(_.id)

      case Left(errors) => fail(s"parsing errors ${errors.toString}")
    }
  }

}
