package com.flowtick.graphs.graphml

import com.flowtick.graphs.layout.DefaultGeometry
import com.flowtick.graphs.{Edge, Graph, Node, Relation}

final case class GraphMLKey(
  id: String,
  name: Option[String] = None,
  typeHint: Option[String] = None,
  targetHint: Option[String] = None,
  yfilesType: Option[String] = None,
  graphsType: Option[String] = None) {}

final case class Fill(color: Option[String], transparent: Boolean = false)

sealed trait LabelModel
case object Custom extends LabelModel
case object Free extends LabelModel

sealed trait LabelLike {
  def text: String
  def textColor: Option[String]
  def fontSize: Option[String]
  def fontFamily: Option[String]
  def model: Option[LabelModel]
  def position: Option[PointSpec]
}

final case class NodeLabel(text: String,
                           textColor: Option[String] = None,
                           fontSize: Option[String] = None,
                           fontFamily: Option[String] = None,
                           modelName: Option[String] = None,
                           position: Option[PointSpec] = None) extends LabelLike {
  override def model: Option[LabelModel] = modelName.map {
    case "custom" => Custom
    case _ => Free
  }
}

final case class BorderStyle(color: String, styleType: String, width: Double)
final case class SVGContent(refId: String)
final case class Image(refId: String)

object ShapeType {
  val Rectangle = "rectangle"
  val RoundRectangle = "roundrectangle"
  val Ellipse = "ellipse"
}

final case class NodeShape(geometry: Option[DefaultGeometry] = None,
                           fill: Option[Fill] = None,
                           label: Option[NodeLabel] = None,
                           shapeType: Option[String] = None,
                           borderStyle: Option[BorderStyle] = None,
                           image: Option[Image] = None,
                           svgContent: Option[SVGContent] = None)

final case class Arrows(source: Option[String], target: Option[String])
final case class EdgeStyle(color: String, width: Option[Double])
final case class EdgeLabel(text: String,
                           textColor: Option[String] = None,
                           fontSize: Option[String] = None,
                           fontFamily: Option[String] = None,
                           model: Option[LabelModel] = Some(Free),
                           position: Option[PointSpec] = None) extends LabelLike

final case class PointSpec(x: Double, y: Double)

final case class EdgePath(sourceX: Double,
                          sourceY: Double,
                          targetX: Double,
                          targetY: Double,
                          points: List[PointSpec])

final case class EdgeShape(label: Option[EdgeLabel] = None,
                           path: Option[EdgePath] = None,
                           edgeStyle: Option[EdgeStyle] = None,
                           arrows: Option[Arrows] = None)

final case class GraphMLResource(id: String, value: String, typeHint: Option[String])

sealed trait GraphMLElement[V] {
  def id: String
  def schemaRef: Option[String]
  def value: V
  def label: Option[LabelLike]
}

final case class GraphMLNode[N](
  id: String,
  value: N,
  shape: Option[NodeShape] = None,
  schemaRef: Option[String] = None) extends GraphMLElement[N] {
  def updateNodeGeometry(x: Double, y: Double): GraphMLNode[N] =
    copy(shape = for {
      shape <- shape
      geo <- shape.geometry
    } yield shape.copy(geometry = Some(DefaultGeometry(x, y, geo.width, geo.height))))

  def updateNodeLabel(textValue: String): GraphMLNode[N] =
    copy(shape = for {
      shape <- shape
    } yield shape.copy(label = shape.label
      .map(_.copy(text = textValue))
      .orElse(Some(NodeLabel(textValue)))))

  def updateValue(update: N => N): GraphMLNode[N] =
    copy(value = update(value))

  def labelValue: Option[String] = shape.flatMap(_.label).map(_.text)

  override def label: Option[LabelLike] = shape.flatMap(_.label)
}

final case class GraphMLEdge[V](
  id: String,
  value: V,
  source: Option[String],
  target: Option[String],
  shape: Option[EdgeShape] = None,
  schemaRef: Option[String] = None) extends GraphMLElement[V] {
  override def label: Option[LabelLike] = shape.flatMap(_.label)

  def updateEdgeLabel(textValue: String): GraphMLEdge[V] =
    copy(shape = for {
      shape <- shape
    } yield shape.copy(label = shape.label
      .map(_.copy(text = textValue))
      .orElse(Some(EdgeLabel(textValue)))))

  def updateValue(update: V => V): GraphMLEdge[V] =
    copy(value = update(value))
}

final case class GraphMLGraph[E, N](graph: Graph[GraphMLEdge[E], GraphMLNode[N]], meta: GraphMLMeta) {
  def updateNode(id: String, update: GraphMLNode[N] => GraphMLNode[N]): GraphMLGraph[E, N] =
    copy(graph = graph.updateNode(id)(update))

  def updateEdge(id: String, update: GraphMLEdge[E] => GraphMLEdge[E]): GraphMLGraph[E, N] =
    copy(graph = graph.updateEdge(id)(update))

  def removeNode(id: String): GraphMLGraph[E, N] = copy(graph = graph.removeNodeById(id))
  def removeEdge(id: String): GraphMLGraph[E, N] = copy(graph = graph.removeEdgeById(id))

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
