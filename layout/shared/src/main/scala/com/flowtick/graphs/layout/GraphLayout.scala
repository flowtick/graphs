package com.flowtick.graphs.layout

import com.flowtick.graphs.layout.GraphLayout.NodeLayout
import com.flowtick.graphs.{Edge, Graph, Labeled, Node}

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
  def layout[E, N](g: Graph[E, N])(implicit edgeLabel: Labeled[Edge[E, N], String]): NodeLayout[Node[N]]
}

object GraphLayout {
  type NodeLayout[Node] = Node => Option[Cell]

  val none: GraphLayout = new GraphLayout {
    def layout[E, N](g: Graph[E, N])(implicit edgeLabel: Labeled[Edge[E, N], String]): NodeLayout[Node[N]] = _ => None
  }
}
