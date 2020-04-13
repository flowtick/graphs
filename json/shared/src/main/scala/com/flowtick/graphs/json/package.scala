package com.flowtick.graphs

import io.circe
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import io.circe.{ Decoder, Encoder, Json }

package object json {

  private[json] final case class JsonGraph[E, N](
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
    def apply[E, N](graph: Graph[E, N])(implicit edgeEncoder: Encoder[E],
                                        nodeEncoder: Encoder[N],
                                        nodeId: Identifiable[N, String],
                                        edgeId: Identifiable[Edge[E, N], String]): Json = JsonGraph(
      edges = graph.edges.map { edge =>
        JsonEdge(edgeId(edge), edge.value, nodeId(edge.from), nodeId(edge.to))
      }.toList,
      nodes = graph.nodes
        .iterator
        .map(node => (nodeId(node), node))
        .toMap).asJson
  }

  object FromJson {
    def apply[E, N](json: String)(implicit
      edgeEncoder: Decoder[E],
      nodeEncoder: Decoder[N]): Either[circe.Error, Graph[E, N]] = {
      decode[JsonGraph[E, N]](json).map(jsonGraph => {
        val edges = jsonGraph.edges.map { edge => edge.
          
        }
        jsonGraph.nodes.values.foldLeft(Graph.empty[E, N])(_ withNode _).withEdges(edges)
      })
    }
  }

}
