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
  properties: Map[String, GraphMLProperty] = Map.empty) extends Node

final case class GraphMLEdge(
  id: String,
  label: Option[String] = None,
  source: GraphMLNode,
  target: GraphMLNode,
  properties: Map[String, GraphMLProperty] = Map.empty) extends DirectedEdge[GraphMLNode]

final case class GraphMLGraph(id: String, graphBuilder: GraphBuilder[GraphMLNode, GraphMLEdge]) extends AbstractGraph[GraphMLNode, GraphMLEdge](graphBuilder)

class GraphMLGraphBuilder(id: String) extends GraphBuilder[GraphMLNode, GraphMLEdge] {
  override def build: GraphMLGraph = GraphMLGraph(id, this)
}

object GraphMLGraph {
  def create(id: String)(block: GraphMLGraphBuilder => Any): GraphMLGraph = {
    val builder = new GraphMLGraphBuilder(id)
    block.apply(builder)
    builder.build
  }
}