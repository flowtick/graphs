package com.flowtick.graphs.layout

import java.util

import com.flowtick.graphs.{ Edge, Graph, Identifiable, Labeled }
import com.mxgraph.layout.hierarchical.mxHierarchicalLayout
import com.mxgraph.model.{ mxCell, mxGeometry, mxGraphModel }
import com.mxgraph.view.mxGraph

import scala.collection.JavaConverters._
import scala.collection.mutable

object JGraphXLayouter extends GraphLayout {
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  override def layout[N, E](g: Graph[N, E], shape: N => Option[ShapeDefinition])(implicit
    identifiable: Identifiable[N],
    edgeLabel: Labeled[E, String]): collection.Map[String, Cell] = {
    new JGraphXLayout[N, E]().layout(g, shape)
      .getModel.asInstanceOf[mxGraphModel]
      .getCells.asScala.mapValues(cell => JGraphXCell(cell.asInstanceOf[mxCell]))
  }
}

final case class JGraphGeometry(geometry: mxGeometry) extends Geometry {
  override def x: Double = geometry.getX

  override def y: Double = geometry.getY

  override def width: Double = geometry.getWidth

  override def height: Double = geometry.getHeight
}

final case class JGraphXCell(cell: mxCell) extends Cell {
  override def geometry: Geometry = JGraphGeometry(cell.getGeometry)
}

class JGraphXLayout[N, E](implicit
  identifiable: Identifiable[N],
  edgeLabel: Labeled[E, String]) {

  def layout(graph: Graph[N, E], shapeDefinition: N => Option[ShapeDefinition]): mxGraph = {
    val layoutGraph = graphToMxGraph(graph, shapeDefinition)

    new mxHierarchicalLayout(layoutGraph).execute(layoutGraph.getDefaultParent)
    layoutGraph
  }

  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  private def graphToMxGraph(graph: Graph[N, E], shapeSpec: N => Option[ShapeDefinition]): mxGraph = {
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
      val id = identifiable.id(node)
      val displayName = id
      val width = shapeSpec(node).map(_.width).getOrElse(30)
      val height = shapeSpec(node).map(_.height).getOrElse(30)
      val vertex = mxGraph.insertVertex(mxGraph.getDefaultParent, id, displayName, 0, 0, width, height)
      vertices.put(id, vertex)

      val style = new util.HashMap[String, Object]()
      shapeSpec(node).foreach(shape => {
        style.put(mxConstants.STYLE_FILLCOLOR, shape.color)
        style.put(mxConstants.STYLE_ROUNDED, shape.rounded.toString)
        style.put(mxConstants.STYLE_SHAPE, shape.shapeType.toLowerCase match {
          case "ellipse" => mxConstants.SHAPE_ELLIPSE
          case "cloud" => mxConstants.SHAPE_CLOUD
          case _ => mxConstants.SHAPE_RECTANGLE
        })
      })
      mxGraph.getStylesheet.putCellStyle(id, style)
      mxGraph.setCellStyle(id, Array[AnyRef](vertex))
    }

    graph.edges.foreach { edge: Edge[E, N] =>
      for {
        source <- edge.predecessors.headOption
        target <- edge.successors.headOption
      } {
        val sourceId = identifiable.id(source)
        val targetId = identifiable.id(target)
        val edgeId = s"$sourceId-$targetId"

        mxGraph.insertEdge(
          mxGraph.getDefaultParent,
          null,
          edgeLabel.label(edge.value).getOrElse(""),
          vertices.get(sourceId).orNull,
          vertices.get(targetId).orNull)
      }
    }

    mxGraph.getModel.endUpdate()
    mxGraph
  }
}
