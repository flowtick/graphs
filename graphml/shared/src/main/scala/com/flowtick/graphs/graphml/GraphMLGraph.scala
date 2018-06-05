package com.flowtick.graphs.graphml

import com.flowtick.graphs._

final case class GraphMLKey(
  id: String,
  name: Option[String] = None,
  typeHint: Option[String] = None,
  targetHint: Option[String] = None,
  yfilesType: Option[String] = None)

final case class GraphMLProperty(key: GraphMLKey, value: Any)

final case class GraphMLNode(
  id: String,
  label: Option[String] = None,
  properties: Map[String, GraphMLProperty] = Map.empty)

final case class GraphMLEdge[N](
  id: String,
  label: Option[String] = None,
  source: N,
  target: Option[N],
  properties: Map[String, GraphMLProperty] = Map.empty)

final case class GraphMLGraph[N, E](
  id: String,
  override val nodes: Set[N],
  edges: Set[E])(implicit edge: Edge[E, N]) extends AbstractGraph[N, E]

object GraphMLGraph {
  def create: Seq[((GraphMLNode, GraphMLNode), Option[String])] => String => GraphMLGraph[GraphMLNode, GraphMLEdge[GraphMLNode]] =
    Graph.create[GraphMLNode, GraphMLEdge[GraphMLNode], GraphMLGraph[GraphMLNode, GraphMLEdge[GraphMLNode]], ((GraphMLNode, GraphMLNode), Option[String]), String]
}
