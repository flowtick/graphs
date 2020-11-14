package com.flowtick.graphs.graphml

import com.flowtick.graphs.layout.{DefaultGeometry, Geometry}
import com.flowtick.graphs.{Edge, Graph, Node, Relation}
import com.flowtick.graphs.style._

final case class GraphMLKey(
  id: String,
  name: Option[String] = None,
  typeHint: Option[String] = None,
  targetHint: Option[String] = None,
  yfilesType: Option[String] = None,
  graphsType: Option[String] = None) {}

final case class GraphMLResource(id: String, value: String, typeHint: Option[String])

sealed trait GraphMLElement[V] {
  def id: String
  def value: V
  def label: Option[LabelStyle]
  def fill: Option[FillLike]
}

final case class GraphMLNode[N](
  id: String,
  value: N,
  shape: Option[NodeShape] = None,
  geometry: Option[Geometry] = None,
  labelValue: Option[String]) extends GraphMLElement[N] {
  def updateNodeGeometry(fx: Double => Double, fy: Double => Double): GraphMLNode[N] =
    copy(geometry = for {
      geo <- geometry
    } yield DefaultGeometry(fx(geo.x), fy(geo.y), geo.width, geo.height))

  def updateNodeLabel(textValue: String): GraphMLNode[N] =
    copy(labelValue = Some(textValue))

  def updateNodeColor(color: String): GraphMLNode[N] =
    copy(shape = for {
      shape <- shape
    } yield shape.copy(fill = shape.fill.map(_.copy(color = Some(color)))))

  def updateValue(update: N => N): GraphMLNode[N] =
    copy(value = update(value))

  override def label: Option[LabelStyle] = shape.flatMap(_.labelStyle)
  override def fill: Option[FillLike] = shape.flatMap(_.fill)
}

final case class GraphMLEdge[V](
  id: String,
  value: V,
  source: Option[String],
  target: Option[String],
  shape: Option[EdgeShape] = None,
  schemaRef: Option[String] = None,
  labelValue: Option[String] = None,
  path: Option[EdgePath] = None) extends GraphMLElement[V] {
  override def label: Option[LabelStyle] = shape.flatMap(_.labelStyle)
  override def fill: Option[FillLike] = shape.map(shape => Fill(shape.edgeStyle.map(_.color)))

  def updateEdgeLabel(textValue: String): GraphMLEdge[V] =
    copy(labelValue = Some(textValue))

  def updateValue(update: V => V): GraphMLEdge[V] =
    copy(value = update(value))
}

final case class GraphMLGraph[E, N](graph: Graph[GraphMLEdge[E], GraphMLNode[N]], meta: GraphMLMeta) {
  def addResource(resource: GraphMLResource): GraphMLGraph[E, N] = copy(meta = meta.copy(resources = meta.resources :+ resource))

  lazy val resourcesById: Map[String, GraphMLResource] = meta.resources.map { resource =>
    resource.id -> resource
  }.toMap
}

final case class GraphMLMeta(id: Option[String] = None,
                             keys: Seq[GraphMLKey] = Seq.empty,
                             resources: Seq[GraphMLResource] = Seq.empty)

object GraphML {
  def empty[E, N]: GraphMLGraph[E, N] = GraphMLGraph[E, N](Graph.empty[GraphMLEdge[E], GraphMLNode[N]], GraphMLMeta())

  def apply[E, N](
    id: String,
    edges: Iterable[Edge[GraphMLEdge[E]]],
    nodes: Iterable[Node[GraphMLNode[N]]] = Iterable.empty,
    keys: Seq[GraphMLKey] = Seq.empty): GraphMLGraph[E, N] = {
    GraphMLGraph(Graph(edges = edges, nodes = nodes), GraphMLMeta(keys = keys))
  }

  def fromEdges[E, N](edges: Iterable[Relation[GraphMLEdge[E], GraphMLNode[N]]]): GraphMLGraph[E, N] =
    GraphMLGraph(Graph.fromEdges(edges), GraphMLMeta())
}
