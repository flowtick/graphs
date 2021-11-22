package com.flowtick.graphs.view

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.flowtick.graphs.Graph
import com.flowtick.graphs.defaults._
import com.flowtick.graphs.defaults.label._
import com.flowtick.graphs.layout.ForceDirectedLayout
import com.flowtick.graphs.style.StyleSheet
import com.flowtick.graphs.style.defaults._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.FileOutputStream

class SVGRendererJvmSpec extends AnyFlatSpec with Matchers {
  "Editor Renderer" should "render node" in {
    val graph: Graph[Unit, String] = Graph.fromEdges(Set("A" --> "B"))

    val renderer = SVGRendererJvm()
    val styleSheet = StyleSheet()

    val renderedSvg = for {
      layout <- IO.fromFuture(IO(ForceDirectedLayout.layout(graph)))
      xmlString <- renderer
        .translateAndScaleView(0, 0, 2)
        .renderGraph(ViewContext(graph, layout, styleSheet))
        .flatMap(_.toXmlString)
      _ <- IO {
        val fileOut = new FileOutputStream("target/test_simple.svg")
        fileOut.write(xmlString.getBytes("UTF-8"))
        fileOut.flush()
        fileOut.close()
      }
    } yield renderer.graphSVG

    renderedSvg.unsafeRunSync()
  }
}
