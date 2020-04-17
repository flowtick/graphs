package com.flowtick.graphs.graphml

import com.flowtick.graphs.layout.{ Geometry, ShapeDefinition }
import com.flowtick.graphs.{ Edge, Graph }

final case class GraphMLKey(
  id: String,
  name: Option[String] = None,
  typeHint: Option[String] = None,
  targetHint: Option[String] = None,
  yfilesType: Option[String] = None,
  graphsType: Option[String] = None)

final case class GraphMLProperty(key: String, value: Any, typeHint: Option[String] = None)

final case class GraphMLNode[N](
  id: String,
  value: N,
  label: Option[String] = None,
  properties: Seq[GraphMLProperty] = Seq.empty,
  shape: Option[ShapeDefinition] = None,
  geometry: Option[Geometry] = None)

final case class GraphMLEdge[V](
  id: String,
  value: V,
  source: Option[String],
  target: Option[String],
  label: Option[String] = None,
  properties: Seq[GraphMLProperty] = Seq.empty)

final case class GraphMLGraph[M](
  meta: M,
  id: Option[String],
  keys: Seq[GraphMLKey])

object GraphML {
  def apply[V, N, M](
    id: String,
    meta: M,
    edges: Iterable[Edge[GraphMLEdge[V], GraphMLNode[N]]],
    nodes: Iterable[GraphMLNode[N]] = Iterable.empty): Graph[GraphMLGraph[M], GraphMLEdge[V], GraphMLNode[N]] = {
    Graph(GraphMLGraph(meta, id = Some(id), keys = Seq.empty), edges = edges, nodes = nodes)
  }

  def fromEdges[V, N](edges: Iterable[Edge[GraphMLEdge[V], GraphMLNode[N]]]): Graph[GraphMLGraph[Unit], GraphMLEdge[V], GraphMLNode[N]] =
    apply("G", (), edges)
}
