package com.flowtick.graphs.graphml

import com.flowtick.graphs.defaults._
import com.flowtick.graphs.defaults.directed._
import com.flowtick.graphs.layout.JGraphXLayouter
import org.scalatest.{ FlatSpec, Matchers }

import scala.xml.Elem

class GraphMLRendererSpec extends FlatSpec with Matchers {
  "GraphRenderer" should "render default graph" in new GraphMLRenderer {
    val newGraph = defaultGraph.from(Set(n("A") --> n("B")))

    val xml: Elem = render(newGraph, JGraphXLayouter)

    xml.headOption shouldBe defined
    xml.headOption.foreach(_.label should be("graphml"))
    xml.descendant.count(_.label == "node") should be(2)
    xml.descendant.count(_.label == "edge") should be(1)

    xml.descendant.filter(_.label == "node").flatMap(_.attribute("id").map(_.text)) should contain theSameElementsAs List("B", "A")
    xml.descendant.filter(_.label == "edge").flatMap(_.attribute("id").map(_.text)) should be(List("A-B"))
  }

}
