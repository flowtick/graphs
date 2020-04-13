package com.flowtick.graphs

final case class JsNode(id: String)
final case class JsEdge(value: Option[String], label: Option[String] = None)
final case class JsGraph(id: Option[String])

object JsGraph {
  implicit def identifiableJsNode: Identifiable[JsNode, String] = new Identifiable[JsNode, String] {
    override def apply(node: JsNode): String = node.id
  }

  implicit def labeledJsEdge: Labeled[JsEdge, String] = new Labeled[JsEdge, String] {
    override def apply(edge: JsEdge): String = edge.label.getOrElse("")
  }
}