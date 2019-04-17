package com.flowtick.graphs.layout

import com.flowtick.graphs.layout.GraphLayout.NodeLayout
import com.flowtick.graphs.{ Edge, Graph, Identifiable, Labeled }

trait Geometry {
  def x: Double
  def y: Double
  def width: Double
  def height: Double
}

case class DefaultGeometry(
  x: Double,
  y: Double,
  width: Double,
  height: Double) extends Geometry

trait Cell {
  def geometry: Geometry
}

trait GraphLayout {
  def layout[V, N, M](g: Graph[V, N, M], shape: N => Option[ShapeDefinition])(implicit
    identifiable: Identifiable[N],
    edgeLabel: Labeled[Edge[V, N], String]): NodeLayout[N]
}

object GraphLayout {
  type NodeLayout[Node] = Node => Option[Cell]

  val none: GraphLayout = new GraphLayout {
    def layout[V, N, M](g: Graph[V, N, M], shape: N => Option[ShapeDefinition])(implicit
      identifiable: Identifiable[N],
      edgeLabel: Labeled[Edge[V, N], String]): NodeLayout[N] = _ => None
  }
}
