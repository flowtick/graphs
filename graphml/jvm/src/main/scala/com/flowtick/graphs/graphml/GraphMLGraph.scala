package com.flowtick.graphs.graphml

import com.flowtick.graphs._

case class GraphMLNode(id: String) extends Node
case class GraphMLEdge(id: String, label: Option[String] = None, source: GraphMLNode, target: GraphMLNode) extends DirectedEdge[GraphMLNode]
case class GraphMLGraph(id: String, graphBuilder: GraphBuilder[GraphMLNode, GraphMLEdge]) extends AbstractGraph[GraphMLNode, GraphMLEdge](graphBuilder)

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