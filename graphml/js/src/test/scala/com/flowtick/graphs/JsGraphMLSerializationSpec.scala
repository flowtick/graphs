package com.flowtick.graphs

import com.flowtick.graphs.graphml._
import org.scalatest.{ FlatSpec, Matchers }

class JsGraphMLSerializationSpec extends FlatSpec with Matchers {
  val testGraph: Graph[GraphMLEdge[Unit], GraphMLNode[Unit], GraphMLGraph[Unit]] = GraphMLGraph(
    id = "new-graph",
    meta = (),
    edges = Set(ml((), Some("A")) --> ml((), Some("B"))))

  val testDataType = new GraphMLDatatype[Unit, Unit, Unit]()

  "GraphRenderer" should "render default graph" in {
    testDataType.serialize(testGraph).headOption match {
      case Some(xml) =>
        xml.headOption shouldBe defined
        xml.headOption.foreach(_.label should be("graphml"))
        xml.descendant.count(_.label == "node") should be(2)
        xml.descendant.count(_.label == "edge") should be(1)

        xml.descendant.filter(_.label == "node").flatMap(_.attribute("id").map(_.text)) should contain theSameElementsAs List("B", "A")
        xml.descendant.filter(_.label == "edge").flatMap(_.attribute("id").map(_.text)) should be(List("A-B"))
      case None => fail
    }
  }

  it should "import graph from XML" in {
    testDataType.serialize(testGraph).headOption match {
      case Some(xml) => fromGraphML[Unit, Unit, Unit](xml.toString()).isRight should be(true)
      case None => fail
    }
  }
}
