package com.flowtick.graphs

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import com.flowtick.graphs.defaults._
import com.flowtick.graphs.defaults.label._
import com.flowtick.graphs.editor.view.SVGRendererJvm
import com.flowtick.graphs.layout.ELkLayoutJVM
import com.flowtick.graphs.style.StyleSheet
import com.flowtick.graphs.style.defaults._
import org.scalatest.AsyncTestSuite
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.FileOutputStream

class SVGRendererJvmSpec extends AnyFlatSpec with Matchers with AsyncTestSuite with AsyncIOSpec {
  "Editor Renderer" should "render node" in {
    val graph: Graph[Unit, String] = Graph.fromEdges(Set("A" --> "B"))

    val renderer = SVGRendererJvm()
    val styleSheet = StyleSheet()

    val renderedSvg = for {
      layout <- IO.fromFuture(IO(ELkLayoutJVM.layout(graph)))
      _ <- renderer.renderGraph(graph, layout, styleSheet)
      xmlString <- IO.fromTry(renderer.toXmlString)
      _ <- IO {
        val fileOut = new FileOutputStream("target/test_simple.svg")
        fileOut.write(xmlString.getBytes("UTF-8"))
        fileOut.flush()
        fileOut.close()
      }
    } yield renderer.graphSVG

    renderedSvg.assertNoException
  }
}
