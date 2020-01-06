package com.flowtick.graphs

import io.circe
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import io.circe.{ Decoder, Encoder, Json }

package object json {

  private[json] final case class JsonGraph[V, N, M](
    meta: M,
    nodes: Map[String, N],
    edges: List[JsonEdge[V]])

  private[json] final case class JsonEdge[V](
    id: String,
    value: V,
    source: String,
    target: String)

  private implicit val unitEncoder = new Encoder[Unit] {
    override def apply(a: Unit): Json = Json.Null
  }

  object ToJson {
    def apply[V, N, M](graph: Graph[V, N, M])(implicit
      edgeEncoder: Encoder[V],
      nodeEncoder: Encoder[N],
      metaEncoder: Encoder[M],
      edgeId: Identifiable[Edge[V, N]],
      nodeId: Identifiable[N]): Json = JsonGraph(
      meta = graph.value,
      edges = graph.edges
        .iterator
        .map(edge => JsonEdge[V](
          edgeId.id(edge),
          edge.value,
          nodeId.id(edge.head),
          nodeId.id(edge.tail)))
        .toList,
      nodes = graph.nodes
        .iterator
        .map(node => (nodeId.id(node), node))
        .toMap).asJson
  }

  object FromJson {
    def apply[V, N, M](json: String)(implicit
      edgeEncoder: Decoder[V],
      nodeEncoder: Decoder[N],
      metaEncoder: Decoder[M]): Either[circe.Error, Graph[V, N, M]] = {
      decode[JsonGraph[V, N, M]](json).map(jsonGraph => {
        Graph.from(
          value = jsonGraph.meta,
          nodes = jsonGraph.nodes.values,
          // FIXME: this throws if edges reference non existing nodes
          edges = jsonGraph.edges.map(edge => Edge[V, N](edge.value, jsonGraph.nodes(edge.source), jsonGraph.nodes(edge.target))))
      })
    }
  }

}
