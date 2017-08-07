package com.flowtick.graphs.layout

import java.util

import com.flowtick.graphs.{ Edge, Graph, Identifiable, Node }
import com.mxgraph.layout.hierarchical.mxHierarchicalLayout
import com.mxgraph.view.mxGraph

import scala.collection.mutable

class JGraphXLayout[N <: Node, E <: Edge[N]](implicit val identifiable: Identifiable[N]) {

  def layout(graph: Graph[N, E], shapeSpec: N => Option[ShapeSpec] = N => None): mxGraph = {
    val layoutGraph = graphToMxGraph(graph, shapeSpec)

    new mxHierarchicalLayout(layoutGraph).execute(layoutGraph.getDefaultParent)
    layoutGraph
  }

  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  private def graphToMxGraph(graph: Graph[N, E], shapeSpec: N => Option[ShapeSpec]): mxGraph = {
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

    graph.edges.foreach { edge =>
      val sourceId = identifiable.id(edge.source)
      val targetId = identifiable.id(edge.target)
      val edgeId = s"$sourceId-$targetId"

      mxGraph.insertEdge(
        mxGraph.getDefaultParent,
        null,
        edge.label.getOrElse(""),
        vertices.get(sourceId).orNull,
        vertices.get(targetId).orNull
      )
    }

    mxGraph.getModel.endUpdate()
    mxGraph
  }
}
