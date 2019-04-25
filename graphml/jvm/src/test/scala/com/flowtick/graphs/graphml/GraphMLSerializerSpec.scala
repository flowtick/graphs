package com.flowtick.graphs.graphml

import cats.data.Validated.Valid
import com.flowtick.graphs.Graph
import org.scalatest.{ FlatSpec, Matchers }

class GraphMLSerializerSpec extends FlatSpec with Matchers {
  case class TestNode(first: String, second: String)

  def prettyPrint(xml: scala.xml.Node) = println(new scala.xml.PrettyPrinter(80, 4).format(xml))

  "GraphRenderer" should "render simple graph" in {

    val newGraph: Graph[GraphMLEdge[Unit], GraphMLNode[TestNode], GraphMLGraph[Unit]] = GraphMLGraph(
      id = "new-graph",
      meta = (),
      edges = Set(ml(TestNode("A", "B"), Some("1")) --> ml(TestNode("C", "D"), Some("2"))))

    implicit val genericTestNode = shapeless.Generic[TestNode]

    new GraphMLSerializer[Unit, TestNode, Unit]().serialize(newGraph).headOption match {
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

  it should "render weighted graph" in {
    val weigthed: Graph[GraphMLEdge[Double], GraphMLNode[TestNode], GraphMLGraph[Unit]] = GraphMLGraph("weighted", (), Set(ml(TestNode("A", "B"), id = Some("1")) --> (42.0, ml(TestNode("C", "D"), id = Some("2")))))

    new GraphMLSerializer[Double, TestNode, Unit]().serialize(weigthed).headOption match {
      case Some(xml) => prettyPrint(xml)
    }
  }

  it should "serialize a product" in {
    case class Foo(bar: String, baz: Double)

    val fooDataType = implicitly[Datatype[Foo]]

    val fooXml = fooDataType.serialize(Foo("bar", 42.0))

    fooXml.toString should be(<value>bar</value><value>42.0</value>.mkString(""))

    val deserialized = fooDataType.deserialize(fooXml)

    deserialized should be(Valid(Foo("bar", 42.0)))
  }

}
