package com.flowtick.graphs.editor

import com.flowtick.graphs.layout.GraphLayoutLike
import com.flowtick.graphs.style.{StyleRef, StyleSheet}
import com.flowtick.graphs.{Edge, Graph, Identifiable, Labeled, Node}
import io.circe.Json

trait EditorGraphElement {
  def id: String
  def data: Json
  def label: Option[String]
  def schemaRef: Option[String]
}

final case class EditorGraphNode(
    id: String,
    data: Json,
    stencil: Option[String],
    schemaRef: Option[String],
    label: Option[String] = None
) extends EditorGraphElement

final case class EditorGraphEdge(
    id: String,
    data: Json,
    connector: Option[String],
    schemaRef: Option[String],
    label: Option[String] = None
) extends EditorGraphElement

final case class EditorGraph(
    graph: Graph[EditorGraphEdge, EditorGraphNode],
    styleSheets: List[Either[String, StyleSheet]],
    layouts: List[Either[String, GraphLayoutLike]],
    schemas: List[Either[String, EditorModel.EditorSchema]]
)

object EditorGraphNode {
  implicit val identifiableEditorNode = new Identifiable[EditorGraphNode] {
    override def apply(value: EditorGraphNode): String = value.id
  }

  implicit val editorNodeStyleRef: StyleRef[Node[EditorGraphNode]] =
    new StyleRef[Node[EditorGraphNode]] {
      override def id(element: Node[EditorGraphNode]): Option[String] = Some(
        element.id
      )
      override def classList(element: Node[EditorGraphNode]): List[String] =
        element.value.stencil.toList
    }

  implicit val editorNodeLabel: Labeled[EditorGraphNode, String] =
    new Labeled[EditorGraphNode, String] {
      override def apply(node: EditorGraphNode): String =
        node.label.getOrElse("")
    }
}

object EditorGraphEdge {
  implicit val editorEdgeStyleRef: StyleRef[Edge[EditorGraphEdge]] =
    new StyleRef[Edge[EditorGraphEdge]] {
      override def id(element: Edge[EditorGraphEdge]): Option[String] = Some(
        element.id
      )
      override def classList(element: Edge[EditorGraphEdge]): List[String] =
        element.value.connector.toList
    }

  implicit val editorEdgeLabel: Labeled[EditorGraphEdge, String] =
    new Labeled[EditorGraphEdge, String] {
      override def apply(edge: EditorGraphEdge): String =
        edge.label.getOrElse("")
    }
}
