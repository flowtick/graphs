package com.flowtick.graphs

import com.flowtick.graphs.defaults._
import com.flowtick.graphs.defaults.directed._
import com.flowtick.graphs.graphml.GraphMLRenderer
import com.flowtick.graphs.layout.GraphLayout.NodeLayout
import com.flowtick.graphs.layout.{ GraphLayout, ShapeDefinition }
import org.scalatest.{ FlatSpec, Matchers }

import scala.xml.Elem

class JsGraphMLExportSpec extends FlatSpec with Matchers {
  "GraphRenderer" should "render default graph" in new GraphMLRenderer {
    val graph = DefaultGraph.create(Seq(
      n("A") -> n("B")))

    val xml: Elem = render(graph, new GraphLayout {
      override def layout[N, E](g: Graph[N, E], shape: N => Option[ShapeDefinition])(implicit
        identifiable: Identifiable[N],
        edgeLabel: Labeled[E, String]): NodeLayout[N] = _ => None
    })

    xml.headOption shouldBe defined
    xml.headOption.foreach(_.label should be("graphml"))
    xml.descendant.count(_.label == "node") should be(2)
    xml.descendant.count(_.label == "edge") should be(1)

    xml.descendant.filter(_.label == "node").flatMap(_.attribute("id").map(_.text)) should contain theSameElementsAs List("B", "A")
    xml.descendant.filter(_.label == "edge").flatMap(_.attribute("id").map(_.text)) should be(List("A-B"))
  }
}
