package com.flowtick.graphs

import cats.effect.IO
import com.flowtick.graphs.defaults._
import com.flowtick.graphs.defaults.label._
import com.flowtick.graphs.editor.view.EditorRendererJvm
import com.flowtick.graphs.layout.ELkLayout
import com.flowtick.graphs.style.StyleSheet
import com.flowtick.graphs.style.defaults._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.FileOutputStream

class EditorRendererJvmSpec extends AnyFlatSpec with Matchers {
  "Editor Renderer" should "render node" in {
    val graph: Graph[Unit, String] = Graph.fromEdges(Set("A" --> "B"))

    val renderer = EditorRendererJvm()
    val layout = ELkLayout.layout(graph)
    val styleSheet = StyleSheet()

    val renderedSvg = for {
      _ <- renderer.renderGraph(graph, layout, styleSheet)
      xmlString <- IO.fromTry(renderer.toXmlString)
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
