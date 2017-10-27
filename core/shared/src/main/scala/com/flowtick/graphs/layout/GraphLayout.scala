package com.flowtick.graphs.layout

import com.flowtick.graphs.{ Edge, Graph, Identifiable, Node }

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
  def layout[N <: Node, E <: Edge[N]](
    g: Graph[N, E],
    shape: N => Option[ShapeDefinition])(implicit identifiable: Identifiable[N]): collection.Map[String, Cell]
}
