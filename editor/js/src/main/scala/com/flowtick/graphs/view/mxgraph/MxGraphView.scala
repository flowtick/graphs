package com.flowtick.graphs.view.mxgraph

import com.flowtick.graphs.{ Edge, Graph, Identifiable, Labeled }
import com.flowtick.graphs.layout.GraphLayout
import com.flowtick.graphs.view.GraphView
import org.scalajs.dom.Element

@SuppressWarnings(Array("org.wartremover.warts.Null"))
class MxGraphView extends GraphView[Element] {
  override def createView[N, E](
    container: Element,
    graphLayout: GraphLayout,
    graph: Graph[N, E])(implicit nodeId: Identifiable[N], edgeLabel: Labeled[E, String]): Unit = {
    MxEvent.disableContextMenu(container)

    val viewGraph = new MxGraph(container)
    val layout = new MxHierarchicalLayout(viewGraph)

    val vertexStyle = viewGraph.getStylesheet().getDefaultVertexStyle()
    vertexStyle.update(MxConstants.STYLE_ROUNDED, true)
    vertexStyle.update(MxConstants.STYLE_FILLCOLOR, "white")
    vertexStyle.update(MxConstants.STYLE_STROKECOLOR, "black")
    vertexStyle.update(MxConstants.STYLE_FONTCOLOR, "black")

    val edgeStyle = viewGraph.getStylesheet().getDefaultEdgeStyle()
    edgeStyle.update(MxConstants.STYLE_STROKECOLOR, "black")

    new MxRubberband(viewGraph)

    viewGraph.getModel().beginUpdate()

    try {
      val parent = viewGraph.getDefaultParent()

      val nodeCells: Map[N, MxCell] = graph.nodes.map { node =>
        val id = nodeId.id(node)
        (node, viewGraph.insertVertex(
          parent = parent,
          id = id,
          value = id,
          x = 0,
          y = 0,
          width = 50,
          height = 25))
      }.toMap

      graph.edges.map { edge =>
        val id = edge.value.toString
        (edge, viewGraph.insertEdge(
          parent = parent,
          id = id,
          value = edgeLabel.label(edge.value).orNull,
          source = edge.left.flatMap(nodeCells.get).orNull,
          target = edge.right.flatMap(nodeCells.get).orNull))
      }

      layout.execute(parent)
    } finally {
      viewGraph.getModel().endUpdate()
    }

  }
}
