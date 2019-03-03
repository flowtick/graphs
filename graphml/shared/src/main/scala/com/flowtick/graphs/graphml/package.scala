package com.flowtick.graphs

package object graphml {
  def nodeProperty(id: String, value: Any, typeHint: Option[String] = None) =
    GraphMLProperty(GraphMLKey(id, targetHint = Some("node"), typeHint = typeHint), value)

  def graphMlNode(id: String, properties: GraphMLProperty*): GraphMLNode =
    GraphMLNode(id, None, properties.map(prop => (prop.key.id, prop)).toMap)

  implicit def identifiable: Identifiable[GraphMLNode] = new Identifiable[GraphMLNode] {
    override def id(node: GraphMLNode): String = node.id
  }

}
