package com.flowtick.graphs.layout

import java.io.{ OutputStream, StringWriter }

import com.mxgraph.util.mxCellRenderer
import com.mxgraph.view.mxGraph
import javax.imageio.ImageIO
import javax.xml.transform.TransformerFactory
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamResult

import scala.util.Try

@SuppressWarnings(Array("org.wartremover.warts.Null", "org.wartremover.warts.Throw"))
object JGraphXLayoutRenderer {

  def renderImage(
    graph: mxGraph,
    output: OutputStream,
    format: String = "PNG",
    scale: Double = 1.0,
    padding: Int = 10
  ): Try[OutputStream] = {
    Try {
      format.toUpperCase() match {
        case "SVG" => saveVectorImage(graph, output, scale, padding)
        case "PNG" | "JPG" => saveRasterImage(graph, output, format, scale, padding)
        case _ => throw new IllegalArgumentException(s"unknown format $format")
      }
    }
  }

  private def saveVectorImage(graph: mxGraph, output: OutputStream, scale: Double, padding: Int) = {
    val document = mxCellRenderer.createSvgDocument(
      graph,
      graph.getChildCells(graph.getModel.getRoot),
      scale,
      null,
      getScaledGraphBounds(graph, scale, padding)
    )

    val transformer = TransformerFactory.newInstance().newTransformer()
    val source = new DOMSource(document)
    val result = new StreamResult(new StringWriter())

    transformer.transform(source, result)

    output.write(result.getWriter.toString.getBytes)
    output.flush()
    output
  }

  private def saveRasterImage(graph: mxGraph, output: OutputStream, format: String, scale: Double, padding: Int) = {
    val image = mxCellRenderer.createBufferedImage(
      graph,
      graph.getChildCells(graph.getModel.getRoot),
      scale,
      null,
      false,
      getScaledGraphBounds(graph, scale, padding)
    )

    ImageIO.write(image, format, output)
    output
  }

  private def getScaledGraphBounds(graph: mxGraph, scale: Double, padding: Int) = {
    val bounds = graph.getGraphBounds
    bounds.setWidth(bounds.getWidth * scale)
    bounds.setHeight(bounds.getHeight * scale)
    bounds.grow(padding)
    bounds
  }

}