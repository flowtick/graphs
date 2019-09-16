package mxgraph

import com.flowtick.graphs._
import com.flowtick.graphs.defaults._
import org.scalajs.dom.Element

// $COVERAGE-OFF$ coverage disabled here due to https://github.com/scoverage/scalac-scoverage-plugin/issues/196
@SuppressWarnings(Array("org.wartremover.warts.Null"))
object MxGraphView {
  def create(
    container: Element,
    graph: Graph[JsEdge, JsNode, JsGraph],
    layout: MxGraph => MxGraph = hierarchicalLayout)(implicit
    nodeId: Identifiable[JsNode],
    edgeLabel: Labeled[Edge[JsEdge, JsNode], String]): MxGraph = {
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
      val nodeCells: Map[JsNode, MxCell] = graph.nodes.map { node =>
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
          value = edgeLabel.label(edge).orNull,
          source = nodeCells.get(edge.head).orNull,
          target = nodeCells.get(edge.tail).orNull))
      }

      layout.execute(parent)
    } finally {
      viewGraph.getModel().endUpdate()
    }

    viewGraph
  }

  def mxGraph(container: Element): MxGraph = {
    MxEvent.disableContextMenu(container)

    val viewGraph = new MxGraph(container)

    val vertexStyle = viewGraph.getStylesheet().getDefaultVertexStyle()
    vertexStyle.update(MxConstants.STYLE_ROUNDED, true)
    vertexStyle.update(MxConstants.STYLE_FILLCOLOR, "white")
    vertexStyle.update(MxConstants.STYLE_STROKECOLOR, "black")
    vertexStyle.update(MxConstants.STYLE_FONTCOLOR, "black")

    val edgeStyle = viewGraph.getStylesheet().getDefaultEdgeStyle()
    edgeStyle.update(MxConstants.STYLE_STROKECOLOR, "black")

    new MxRubberband(viewGraph)
    viewGraph
  }

  def hierarchicalLayout: MxGraph => MxGraph = { viewGraph =>
    println("running layout...")
    new MxHierarchicalLayout(viewGraph).execute(viewGraph.getDefaultParent())
    viewGraph
  }

  def toGraph[G[_, _, _]](meta: JsGraph, view: MxGraph)(implicit identifiable: Identifiable[JsNode]): Graph[JsEdge, JsNode, JsGraph] = {

    val nodes = view.getModel().getChildVertices(view.getDefaultParent()).map(nodeCell => JsNode(nodeCell.getId()))
    val edges = view.getModel().getChildEdges(view.getDefaultParent()).map(edgeCell => {
      Edge(
        JsEdge(Some(edgeCell.getId())),
        JsNode(edgeCell.source.get.getId()),
        JsNode(edgeCell.target.get.getId()))
    })

    Graph.create[JsEdge, JsNode, JsGraph](meta, nodes, edges)
  }
}
// $COVERAGE-ON$
