package com.flowtick.graphs.editor

import com.flowtick.graphs.Graph
import com.flowtick.graphs.layout.{DefaultGeometry, Geometry}
import com.flowtick.graphs.style.{EdgePath, StyleSheet}
import io.circe.Json

trait EditorGraphLayoutLike {
  def layouts: List[EditorGraphLayout]
  def setNodeGeometry(id: String, geometry: Geometry): EditorGraphLayoutLike
  def updateNodePosition(id: String, fx: Double => Double, fy: Double => Double): EditorGraphLayoutLike
  def setEdgePath(id: String, edgePath: EdgePath): EditorGraphLayoutLike
  def nodeGeometry(id: String): Option[Geometry]
  def edgePath(id: String): Option[EdgePath]

  def ++(other: List[EditorGraphLayout]): EditorGraphLayoutLike
}

final case class EditorGraphLayout(nodes: Map[String, Geometry] = Map.empty,
                                   edges: Map[String, EdgePath] = Map.empty) extends EditorGraphLayoutLike {
  override def setNodeGeometry(id: String, geometry: Geometry): EditorGraphLayout =
    copy(nodes = nodes + (id -> geometry))

  override def updateNodePosition(id: String, fx: Double => Double, fy: Double => Double): EditorGraphLayout =
    copy(nodes = nodes.get(id).map(geo => nodes + (id -> DefaultGeometry(x = fx(geo.x), y = fy(geo.y), geo.width, geo.height))).getOrElse(nodes))

  override def setEdgePath(id: String, edgePath: EdgePath): EditorGraphLayout =
    copy(edges = edges + (id -> edgePath))

  override def nodeGeometry(id: String): Option[Geometry] = nodes.get(id)

  override def edgePath(id: String): Option[EdgePath] = edges.get(id)

  override def layouts: List[EditorGraphLayout] = List(this)

  override def ++(other: List[EditorGraphLayout]): EditorGraphLayoutLike =
    EditorGraphLayouts(List(this) ++ other)
}

final case class EditorGraphLayouts(layouts: List[EditorGraphLayout]) extends EditorGraphLayoutLike {
  private def updateFirst(update: EditorGraphLayout => EditorGraphLayout): EditorGraphLayouts =
    copy(layouts = layouts.headOption.orElse(Some(EditorGraphLayout())).map(first => update(first) :: layouts.tail).getOrElse(layouts))

  override def setNodeGeometry(id: String, geometry: Geometry): EditorGraphLayoutLike =
    updateFirst(_.setNodeGeometry(id, geometry))

  override def updateNodePosition(id: String, fx: Double => Double, fy: Double => Double): EditorGraphLayoutLike =
    updateFirst(_.updateNodePosition(id, fx, fy))

  override def setEdgePath(id: String, edgePath: EdgePath): EditorGraphLayoutLike =
    updateFirst(_.setEdgePath(id, edgePath))

  override def nodeGeometry(id: String): Option[Geometry] =
    layouts.view.find(_.nodeGeometry(id).nonEmpty).flatMap(_.nodeGeometry(id))

  override def edgePath(id: String): Option[EdgePath] =
    layouts.view.find(_.edgePath(id).nonEmpty).flatMap(_.edgePath(id))

  override def ++(other: List[EditorGraphLayout]): EditorGraphLayoutLike =
    copy(layouts = layouts ++ other)
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
                             styleSheets: List[Either[String, StyleSheet]],
                             layouts: List[Either[String, EditorGraphLayout]],
                             schemas: List[Either[String, EditorModel.EditorSchema]])
