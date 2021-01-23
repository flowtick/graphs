package com.flowtick.graphs.layout

import com.flowtick.graphs.{Edge, Graph, Labeled}

trait Geometry {
  def x: Double
  def y: Double
  def width: Double
  def height: Double
}

trait PointLike {
  def x: Double
  def y: Double
}

case class DefaultGeometry(
  x: Double,
  y: Double,
  width: Double,
  height: Double) extends Geometry

trait Cell {
  def geometry: Option[Geometry]
}

final case class EdgePath(sourceX: Double = 0.0,
                          sourceY: Double = 0.0,
                          targetX: Double = 0.0,
                          targetY: Double = 0.0,
                          points: List[PointSpec] = List.empty)

final case class PointSpec(x: Double, y: Double)

trait GraphLayoutLike {
  def layouts: List[GraphLayout]
  def ++(other: List[GraphLayout]): GraphLayoutLike

  def nodeGeometry(id: String): Option[Geometry]
  def setNodeGeometry(id: String, geometry: Geometry): GraphLayoutLike

  def updateNodePosition(id: String, fx: Double => Double, fy: Double => Double): GraphLayoutLike

  def edgePath(id: String): Option[EdgePath]
  def setEdgePath(id: String, edgePath: EdgePath): GraphLayoutLike
}

final case class GraphLayout(nodes: Map[String, Geometry] = Map.empty,
                             edges: Map[String, EdgePath] = Map.empty) extends GraphLayoutLike {
  override def setNodeGeometry(id: String, geometry: Geometry): GraphLayout =
    copy(nodes = nodes + (id -> geometry))

  override def updateNodePosition(id: String, fx: Double => Double, fy: Double => Double): GraphLayout =
    copy(nodes = nodes.get(id).map(geo => nodes + (id -> DefaultGeometry(x = fx(geo.x), y = fy(geo.y), geo.width, geo.height))).getOrElse(nodes))

  override def setEdgePath(id: String, edgePath: EdgePath): GraphLayout =
    copy(edges = edges + (id -> edgePath))

  override def nodeGeometry(id: String): Option[Geometry] = nodes.get(id)

  override def edgePath(id: String): Option[EdgePath] = edges.get(id)

  override def layouts: List[GraphLayout] = List(this)

  override def ++(other: List[GraphLayout]): GraphLayoutLike = GraphLayouts(List(this) ++ other)
}

final case class GraphLayouts(layouts: List[GraphLayout] = List.empty) extends GraphLayoutLike {
  private def updateFirst(update: GraphLayout => GraphLayout): GraphLayouts = {
    val updatedLayouts = layouts match {
      case Nil => List(update(GraphLayout()))
      case head :: xs => update(head) :: xs
    }
    copy(layouts = updatedLayouts)
  }

  override def setNodeGeometry(id: String, geometry: Geometry): GraphLayoutLike =
    updateFirst(_.setNodeGeometry(id, geometry))

  override def updateNodePosition(id: String, fx: Double => Double, fy: Double => Double): GraphLayoutLike =
    updateFirst(_.updateNodePosition(id, fx, fy))

  override def setEdgePath(id: String, edgePath: EdgePath): GraphLayoutLike =
    updateFirst(_.setEdgePath(id, edgePath))

  override def nodeGeometry(id: String): Option[Geometry] =
    layouts.view.find(_.nodeGeometry(id).nonEmpty).flatMap(_.nodeGeometry(id))

  override def edgePath(id: String): Option[EdgePath] =
    layouts.view.find(_.edgePath(id).nonEmpty).flatMap(_.edgePath(id))

  override def ++(other: List[GraphLayout]): GraphLayoutLike =
    copy(layouts = layouts ++ other)
}

sealed trait LayoutDirection
case object Up extends LayoutDirection
case object Down extends LayoutDirection
case object Left extends LayoutDirection
case object Right extends LayoutDirection

final case class GraphLayoutConfiguration(nodeWidth: Double = 80,
                                          nodeHeight: Double = 40,
                                          spacing: Option[Double] = None,
                                          spacingNodeNode: Option[Double] = None,
                                          direction: Option[LayoutDirection] = None)

trait GraphLayoutOps {
  def layout[E, N](g: Graph[E, N], layoutConfiguration: GraphLayoutConfiguration = GraphLayoutConfiguration())(implicit edgeLabel: Labeled[Edge[E], String]): GraphLayoutLike
}

object GraphLayoutOps {
  val none: GraphLayoutOps = new GraphLayoutOps {
    def layout[E, N](g: Graph[E, N], layoutConfiguration: GraphLayoutConfiguration)(implicit edgeLabel: Labeled[Edge[E], String]): GraphLayout = GraphLayout()
  }
}
