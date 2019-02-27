package mxgraph

import com.flowtick.graphs._
import org.scalajs.dom.Element

@SuppressWarnings(Array("org.wartremover.warts.Null"))
object MxGraphView {
  def create[G[_, _, _], E[_, _]](
    container: Element,
    graph: G[E[JsEdge, JsNode], JsNode, JsGraph],
    layout: MxGraph => MxGraph = hierarchicalLayout)(implicit
    graphType: Graph[G, E],
    edgeType: EdgeType[E],
    nodeId: Identifiable[JsNode],
    edgeLabel: Labeled[E[JsEdge, JsNode], String]): MxGraph = {
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
      val nodeCells: Map[JsNode, MxCell] = graphType.nodes[JsEdge, JsNode, JsGraph](graph).map { node =>
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

      graphType.edges(graph).map { edge =>
        val id = edgeType.value(edge).toString
        (edge, viewGraph.insertEdge(
          parent = parent,
          id = id,
          value = edgeLabel.label(edge).orNull,
          source = nodeCells.get(edgeType.head(edge)).orNull,
          target = nodeCells.get(edgeType.tail(edge)).orNull))
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

  def toGraph[G[_, _, _], E[_, _]](meta: JsGraph, view: MxGraph)(implicit
    builder: GraphBuilder[G, E],
    edge: EdgeType[E],
    identifiable: Identifiable[JsNode]): G[E[JsEdge, JsNode], JsNode, JsGraph] = {

    val nodes = view.getModel().getChildVertices(view.getDefaultParent()).map(nodeCell => JsNode(nodeCell.getId()))
    val edges = view.getModel().getChildEdges(view.getDefaultParent()).map(edgeCell => {
      edge.apply(
        JsEdge(Some(edgeCell.getId())),
        JsNode(edgeCell.source.get.getId()),
        JsNode(edgeCell.target.get.getId()))
    })

    builder.create[JsEdge, JsNode, JsGraph](meta, nodes, edges)
  }
}
