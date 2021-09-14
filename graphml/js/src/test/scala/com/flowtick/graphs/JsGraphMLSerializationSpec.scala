package com.flowtick.graphs

import com.flowtick.graphs.graphml._
import com.flowtick.graphs.graphml.generic._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class JsGraphMLSerializationSpec extends AnyFlatSpec with Matchers {
  val testGraph: GraphMLGraph[Unit, Unit] =
    GraphML.fromEdges(Set(ml((), Some("A")) --> ml((), Some("B"))))

  val testDataType = GraphMLDatatype[Unit, Unit]

  "GraphRenderer" should "render default graph" in {
    testDataType.serialize(testGraph, None).headOption match {
      case Some(xml) =>
        xml.headOption shouldBe defined
        xml.headOption.foreach(_.label should be("graphml"))
        xml.descendant.count(_.label == "node") should be(2)
        xml.descendant.count(_.label == "edge") should be(1)

        xml.descendant
          .filter(_.label == "node")
          .flatMap(
            _.attribute("id").map(_.text)
          ) should contain theSameElementsAs List("B", "A")
        xml.descendant
          .filter(_.label == "edge")
          .flatMap(_.attribute("id").map(_.text)) should be(List("A-B"))
      case None => fail
    }
  }

  it should "import graph from XML" in {
    testDataType.serialize(testGraph, None).headOption match {
      case Some(xml) =>
        val loaded = FromGraphML[Unit, Unit](xml.toString()).getOrElse(fail())
        loaded.graph.edges.size should be(1)
        loaded.graph.nodes.size should be(2)
      case None => fail
    }
  }
}
