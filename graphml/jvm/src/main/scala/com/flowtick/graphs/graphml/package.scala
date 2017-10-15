package com.flowtick.graphs

package object graphml {
  def node(id: String, properties: Map[String, Any] = Map.empty): GraphMLNode = GraphMLNode(id, properties)

  implicit def identifiable: Identifiable[GraphMLNode] = new Identifiable[GraphMLNode] {
    override def id(node: GraphMLNode): String = node.id
  }

  implicit class GraphMLNodeOps(n: GraphMLNode) extends NodeOps[GraphMLNode, GraphMLEdge]
    with DirectedNodeOps[GraphMLNode, GraphMLEdge] {
    val node: GraphMLNode = n

    override def ~>(target: GraphMLNode)(implicit graphBuilder: GraphBuilder[GraphMLNode, GraphMLEdge]): GraphMLNode = {
      graphBuilder.addEdge(GraphMLEdge(s"${n.id}-${target.id}", None, node, target))
      target
    }
  }
}
