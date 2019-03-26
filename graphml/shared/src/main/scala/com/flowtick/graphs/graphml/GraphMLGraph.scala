package com.flowtick.graphs.graphml

final case class GraphMLKey(
  id: String,
  name: Option[String] = None,
  typeHint: Option[String] = None,
  targetHint: Option[String] = None,
  yfilesType: Option[String] = None)

final case class GraphMLProperty(key: GraphMLKey, value: Any)

final case class GraphMLNode[N](
  id: String,
  value: N,
  label: Option[String] = None,
  properties: Map[String, GraphMLProperty] = Map.empty)

final case class GraphMLEdge[V](
  id: String,
  value: V,
  label: Option[String] = None,
  properties: Map[String, GraphMLProperty] = Map.empty)

final case class GraphMLGraph(
  id: Option[String])
