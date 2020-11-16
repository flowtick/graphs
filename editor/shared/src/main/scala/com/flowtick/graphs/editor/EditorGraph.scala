package com.flowtick.graphs.editor

import com.flowtick.graphs.Graph
import com.flowtick.graphs.json.schema.Schema
import com.flowtick.graphs.layout.{DefaultGeometry, Geometry}
import com.flowtick.graphs.style.{EdgePath, StyleSheet}
import io.circe.Json

final case class EditorGraphLayout(nodes: Map[String, Geometry] = Map.empty,
                                   edges: Map[String, EdgePath] = Map.empty) {
  def setNodeGeometry(id: String, geometry: Geometry): EditorGraphLayout =
    copy(nodes = nodes + (id -> geometry))

  def updateNodePosition(id: String, fx: Double => Double, fy: Double => Double): EditorGraphLayout =
    copy(nodes = nodes.get(id).map(geo => nodes + (id -> DefaultGeometry(x = fx(geo.x), y = fy(geo.y), geo.width, geo.height))).getOrElse(nodes))

  def setEdgePath(id: String, edgePath: EdgePath): EditorGraphLayout =
    copy(edges = edges + (id -> edgePath))
}

trait EditorGraphElement {
  def data: Json
  def label: Option[String]
  def schemaRef: Option[String]
}

final case class EditorGraphNode(data: Json,
                                 stencil: Option[String],
                                 schemaRef: Option[String],
                                 label: Option[String] = None) extends EditorGraphElement

final case class EditorGraphEdge(data: Json,
                                 connector: Option[String],
                                 schemaRef: Option[String],
                                 label: Option[String] = None) extends EditorGraphElement

final case class EditorGraph(graph: Graph[EditorGraphEdge, EditorGraphNode],
                             styleSheet: StyleSheet,
                             layout: EditorGraphLayout,
                             schema: EditorModel.EditorSchema) {
  def updateNode(id: String, update: EditorGraphNode => EditorGraphNode): EditorGraph =
    copy(graph = graph.updateNode(id)(update))

  def updateEdge(id: String, update: EditorGraphEdge => EditorGraphEdge): EditorGraph =
    copy(graph = graph.updateEdge(id)(update))

  def updateStyleSheet(update: StyleSheet => StyleSheet): EditorGraph =
    copy(styleSheet = update(styleSheet))

  def updateLayout(update: EditorGraphLayout => EditorGraphLayout): EditorGraph =
    copy(layout = update(layout))

  def updateSchema(update: EditorModel.EditorSchema => EditorModel.EditorSchema): EditorGraph =
    copy(schema = update(schema))

  def removeNode(id: String): EditorGraph = copy(graph = graph.removeNodeById(id))
  def removeEdge(id: String): EditorGraph = copy(graph = graph.removeEdgeById(id))
}

object EditorGraph {
  val empty: EditorGraph = EditorGraph(Graph.empty, StyleSheet.empty, EditorGraphLayout(), Schema[EditorSchemaHints]())
}
