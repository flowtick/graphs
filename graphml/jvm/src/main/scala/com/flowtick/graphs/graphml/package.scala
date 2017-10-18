package com.flowtick.graphs

package object graphml {
  def nodeProperty(id: String, value: Any, typeHint: Option[String] = None) =
    GraphMLProperty(GraphMLKey(id, targetHint = Some("node"), typeHint = typeHint), value)

  def node(id: String, properties: GraphMLProperty*): GraphMLNode =
    GraphMLNode(id, None, properties.map(prop => (prop.key.id, prop)).toMap)

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
