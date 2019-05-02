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

final case class GraphMLProperty(key: String, value: Any)

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
  label: Option[String] = None,
  properties: Seq[GraphMLProperty] = Seq.empty)

final case class GraphMLGraph[M](
  meta: M,
  id: Option[String],
  keys: Seq[GraphMLKey])

object GraphMLGraph {
  def apply[V, N, M](
    id: String,
    meta: M,
    edges: Iterable[Edge[GraphMLEdge[V], GraphMLNode[N]]],
    nodes: Option[Iterable[GraphMLNode[N]]] = None): Graph[GraphMLEdge[V], GraphMLNode[N], GraphMLGraph[M]] = {
    Graph.of(GraphMLGraph(meta, id = Some(id), keys = Seq.empty), nodes)(edges)
  }
}
