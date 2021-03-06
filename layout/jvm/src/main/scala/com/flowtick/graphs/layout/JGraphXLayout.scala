package com.flowtick.graphs.layout

import java.util

import com.flowtick.graphs.layout.GraphLayout.NodeLayout
import com.flowtick.graphs.{Edge, Graph, Identifiable, Labeled, Node}
import com.mxgraph.layout.hierarchical.mxHierarchicalLayout
import com.mxgraph.model.{mxCell, mxGeometry, mxGraphModel}
import com.mxgraph.view.mxGraph

import scala.collection.JavaConverters._
import scala.collection.mutable

object JGraphXLayouter extends GraphLayout {
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  override def layout[E, N](g: Graph[E, N])(implicit edgeLabel: Labeled[Edge[E], String]): NodeLayout[Node[N]] = {
    val cells: Map[String, JGraphXCell] = new JGraphXLayout[E, N]()
      .layout(g)
      .getModel.asInstanceOf[mxGraphModel]
      .getCells.asScala.mapValues(cell => JGraphXCell(cell.asInstanceOf[mxCell]))
      .toMap
    
    (node: Node[N]) => cells.get(node.id)
  }
}

final case class JGraphGeometry(geometry: mxGeometry) extends Geometry {
  override def x: Double = geometry.getX
  override def y: Double = geometry.getY
  override def width: Double = geometry.getWidth
  override def height: Double = geometry.getHeight
}

final case class JGraphXCell(cell: mxCell) extends Cell {
  override def geometry: Option[Geometry] = Option(cell.getGeometry).map(JGraphGeometry)
}

class JGraphXLayout[E, N](implicit edgeLabel: Labeled[Edge[E], String]) {

  def layout(graph: Graph[E, N]): mxGraph = {
    val layoutGraph = graphToMxGraph(graph)

    new mxHierarchicalLayout(layoutGraph).execute(layoutGraph.getDefaultParent)
    layoutGraph
  }

  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  private def graphToMxGraph(graph: Graph[E, N]): mxGraph = {
    val mxGraph = new mxGraph
    mxGraph.getModel.beginUpdate()

    import com.mxgraph.util.mxConstants
    val styleSheet = mxGraph.getStylesheet

    styleSheet.getDefaultVertexStyle.put(mxConstants.STYLE_FILLCOLOR, "#FFFFFF")
    styleSheet.getDefaultVertexStyle.put(mxConstants.STYLE_STROKECOLOR, "#000000")
    styleSheet.getDefaultVertexStyle.put(mxConstants.STYLE_FONTCOLOR, "#000000")

    styleSheet.getDefaultEdgeStyle.put(mxConstants.STYLE_STROKECOLOR, "#000000")
    styleSheet.getDefaultEdgeStyle.put(mxConstants.STYLE_FONTCOLOR, "#000000")

    val vertices = mutable.Map[String, Object]()

    graph.nodes.foreach { node =>
      val id: String = node.id
      val displayName = id
      val width = 30
      val height = 30
      val vertex = mxGraph.insertVertex(mxGraph.getDefaultParent, id, displayName, 0, 0, width, height)
      vertices.put(id, vertex)

      val style = new util.HashMap[String, Object]()
      style.put(mxConstants.STYLE_FILLCOLOR, "#FFFFFF")
      style.put(mxConstants.STYLE_ROUNDED, "true")
      style.put(mxConstants.STYLE_SHAPE, mxConstants.SHAPE_RECTANGLE)

      mxGraph.getStylesheet.putCellStyle(id, style)
      mxGraph.setCellStyle(id, Array[AnyRef](vertex))
    }

    for {
      edge <- graph.edges
    } yield {
        mxGraph.insertEdge(
          mxGraph.getDefaultParent,
          edge.id,
          edgeLabel(edge),
          vertices.get(edge.from).orNull,
          vertices.get(edge.to).orNull)
    }

    mxGraph.getModel.endUpdate()
    mxGraph
  }
}
