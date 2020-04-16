  package com.flowtick.graphs

import io.circe
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import io.circe.{ Decoder, Encoder, Json }

package object json {

  private[json] final case class JsonGraph[E, N, M](
    meta: M,
    nodes: Map[String, N],
    edges: List[JsonEdge[E]])

  private[json] final case class JsonEdge[E](
    id: String,
    value: E,
    source: String,
    target: String)

  private implicit val unitEncoder = new Encoder[Unit] {
    override def apply(a: Unit): Json = Json.Null
  }

  object ToJson {
    def apply[E, N, M](graph: Graph[E, N, M])(implicit edgeEncoder: Encoder[E],
                                              nodeEncoder: Encoder[N],
                                              metaEncoder: Encoder[M],
                                              nodeId: Identifiable[N, String],
                                              edgeId: Identifiable[Edge[E, N], String]): Json = JsonGraph(
      meta = graph.meta,
      edges = graph.edges.map { edge =>
        JsonEdge(edgeId(edge), edge.value, nodeId(edge.from), nodeId(edge.to))
      }.toList,
      nodes = graph.nodes
        .iterator
        .map(node => (nodeId(node), node))
        .toMap).asJson
  }

  object FromJson {
    def apply[E, N, M](json: String)(implicit edgeEncoder: Decoder[E],
                                     nodeEncoder: Decoder[N],
                                     metaDecoder: Decoder[M]): Either[circe.Error, Graph[E, N, M]] = {
      decode[JsonGraph[E, N, M]](json).map(jsonGraph => {
        val edges: List[Edge[E, N]] = jsonGraph.edges.map { edge =>
          Edge(edge.value, jsonGraph.nodes(edge.source), jsonGraph.nodes(edge.target))
        }
        jsonGraph.nodes.values.foldLeft(Graph.empty[E, N, M](jsonGraph.meta))(_ withNode _).withEdges(edges)
      })
    }
  }

}
