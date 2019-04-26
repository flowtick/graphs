package com.flowtick.graphs

import com.flowtick.graphs.defaults._
import com.flowtick.graphs.graphml.GraphMLDatatype
import com.flowtick.graphs.layout.GraphLayout
import org.scalatest.{ FlatSpec, Matchers }

import scala.xml.Elem

class JsGraphMLExportSpec extends FlatSpec with Matchers {
  "GraphRenderer" should "render default graph" in new GraphMLDatatype {
    val graph = Graph.from(Seq(
      n("A") --> n("B")))

    val xml: Elem = render(graph, GraphLayout.none)

    xml.headOption shouldBe defined
    xml.headOption.foreach(_.label should be("graphml"))
    xml.descendant.count(_.label == "node") should be(2)
    xml.descendant.count(_.label == "edge") should be(1)

    xml.descendant.filter(_.label == "node").flatMap(_.attribute("id").map(_.text)) should contain theSameElementsAs List("B", "A")
    xml.descendant.filter(_.label == "edge").flatMap(_.attribute("id").map(_.text)) should be(List("A-B"))
  }
}
