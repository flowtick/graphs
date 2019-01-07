package com.flowtick.graphs.layout

import com.flowtick.graphs.layout.GraphLayout.NodeLayout
import com.flowtick.graphs.{ Graph, Identifiable, Labeled }

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
  def layout[N, E](
    g: Graph[N, E],
    shape: N => Option[ShapeDefinition])(implicit
    identifiable: Identifiable[N],
    edgeLabel: Labeled[E, String]): NodeLayout[N]
}

object GraphLayout {
  type NodeLayout[Node] = Node => Option[Cell]

  val none: GraphLayout = new GraphLayout {
    override def layout[N, E](g: Graph[N, E], shape: N => Option[ShapeDefinition])(implicit identifiable: Identifiable[N], edgeLabel: Labeled[E, String]): NodeLayout[N] =
      (_: N) => None
  }
}
