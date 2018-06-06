package com.flowtick.graphs

package object graphml {
  def nodeProperty(id: String, value: Any, typeHint: Option[String] = None) =
    GraphMLProperty(GraphMLKey(id, targetHint = Some("node"), typeHint = typeHint), value)

  def node(id: String, properties: GraphMLProperty*): GraphMLNode =
    GraphMLNode(id, None, properties.map(prop => (prop.key.id, prop)).toMap)

  implicit def identifiable: Identifiable[GraphMLNode] = new Identifiable[GraphMLNode] {
    override def id(node: GraphMLNode): String = node.id
  }

  implicit def edgeBuilder[N]: EdgeBuilder[N, GraphMLEdge[N], ((N, N), Option[String])] = new EdgeBuilder[N, GraphMLEdge[N], ((N, N), Option[String])] {
    override def create(from: ((N, N), Option[String]))(implicit identifiable: Identifiable[N]): GraphMLEdge[N] = GraphMLEdge(
      id = s"${identifiable.id(from._1._1)}-${identifiable.id(from._1._2)}",
      label = from._2,
      source = from._1._1,
      target = Some(from._1._2))
  }

  implicit def singleNodeEdgeBuilder[N]: EdgeBuilder[N, GraphMLEdge[N], (N, Option[String])] = new EdgeBuilder[N, GraphMLEdge[N], (N, Option[String])] {
    override def create(from: (N, Option[String]))(implicit identifiable: Identifiable[N]): GraphMLEdge[N] = GraphMLEdge(
      id = s"${identifiable.id(from._1)}",
      label = from._2,
      source = from._1,
      target = None)
  }

  implicit def edgeLabel[N]: Labeled[GraphMLEdge[N], String] = new Labeled[GraphMLEdge[N], String] {
    override def label(graphMLEdge: GraphMLEdge[N]): Option[String] = graphMLEdge.label
  }
}
