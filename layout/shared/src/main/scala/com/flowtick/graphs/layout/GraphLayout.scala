package com.flowtick.graphs.layout

import com.flowtick.graphs.layout.GraphLayout.NodeLayout
import com.flowtick.graphs.{Edge, Graph, Identifiable, Labeled}

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
  def geometry: Option[Geometry]
}

trait GraphLayout {
  def layout[E, N, M](g: Graph[E, N, M])(implicit edgeLabel: Labeled[Edge[E, N], String],
                                   edgeId: Identifiable[Edge[E, N], String],
                                   nodeId: Identifiable[N, String]): NodeLayout[N]
}

object GraphLayout {
  type NodeLayout[Node] = Node => Option[Cell]

  val none: GraphLayout = new GraphLayout {
    def layout[E, N, M](g: Graph[E, N, M])(implicit edgeLabel: Labeled[Edge[E, N], String],
                                     edgeId: Identifiable[Edge[E, N], String],
                                     nodeId: Identifiable[N, String]): NodeLayout[N] = _ => None
  }
}
