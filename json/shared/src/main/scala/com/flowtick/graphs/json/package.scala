package com.flowtick.graphs

import io.circe
import io.circe.Decoder.Result
import io.circe.{ Decoder, Encoder, HCursor, Json }
import io.circe.parser._
import io.circe.syntax._
import io.circe.generic.auto._

package object json {

  private[json] final case class JsonGraph[V, N, M](
    meta: M,
    nodes: List[JsonNode[N]],
    edges: List[JsonEdge[V]])

  private[json] final case class JsonNode[N](id: String, value: N)

  private[json] final case class JsonEdge[V](
    id: String,
    value: V,
    source: String,
    target: String)

  implicit val unitEncoder = new Encoder[Unit] {
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
        .map(node => JsonNode(nodeId.id(node), node))
        .toList).asJson
  }

  object FromJson {
    def apply[V, N, M](json: String)(implicit
      edgeEncoder: Decoder[V],
      nodeEncoder: Decoder[N],
      metaEncoder: Decoder[M]) = {
      decode[JsonGraph[V, N, M]](json).map(jsonGraph => {
        Graph.from(
          jsonGraph.meta,
          nodes = jsonGraph.nodes.map(_.value),
          edges = jsonGraph.edges.map(edge => Edge[V, N](edge.value, ???, ???)))
      })
    }
  }

}
