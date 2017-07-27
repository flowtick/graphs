package com.flowtick.graphs.graphml

import com.flowtick.graphs.defaults._
import org.scalatest.{FlatSpec, Matchers}

import scala.xml.Elem

class GraphMlRendererSpec extends FlatSpec with Matchers {
  "GraphRenderer" should "render default graph" in new GraphMlRenderer {
    val xml: Elem = render(DefaultGraph.create { implicit graph =>
      n("A") ~> n("B")
    })

    xml.headOption shouldBe defined
    xml.headOption.foreach(_.label should be("graphml"))
    xml.descendant.count(_.label == "node") should be(2)
    xml.descendant.count(_.label == "edge") should be(1)

    xml.descendant.filter(_.label == "node").flatMap(_.attribute("id").map(_.text)) should be(List("B", "A"))
    xml.descendant.filter(_.label == "edge").flatMap(_.attribute("id").map(_.text)) should be(List("A-B"))
  }

}
