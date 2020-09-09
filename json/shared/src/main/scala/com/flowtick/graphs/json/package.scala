  package com.flowtick.graphs

import io.circe
import io.circe.Decoder.Result
import io.circe.parser._
import io.circe.syntax._
import io.circe.{Codec, Decoder, Encoder, HCursor, Json}

import scala.collection.mutable.ListBuffer

package object json {

  final case class JsonGraph[M, E, N](
    graph: Graph[E, N],
    meta : Option[M] = None)

  final case class JsonEdge[E](id: String, value: E, from: String, to: String)

  object ToJson {
    def apply[M, E, N](graph: Graph[E, N],
                       meta : Option[M] = None)(implicit jsonGraphEncoder: Encoder[JsonGraph[M, E, N]]): Json =
      JsonGraph(graph, meta).asJson
  }

  object FromJson {
    def apply[M, E, N](json: String)(implicit jsonGraphDecoder: Decoder[JsonGraph[M, E, N]]): Either[circe.Error, JsonGraph[M, E, N]] =
      decode[JsonGraph[M, E, N]](json)
  }

  object format {
    implicit object default {
      implicit val unitAsNull: Codec[Unit] = new Codec[Unit] {
        override def apply(a: Unit): Json = Json.Null
        override def apply(c: HCursor): Result[Unit] = Right(())
      }

      import io.circe.generic.semiauto._

      implicit def nodeEncoder[N](implicit nodeEncoder: Encoder[N]): Encoder[Node[N]] = deriveEncoder[Node[N]]
      implicit def nodeDecoder[N](implicit nodeDecoder: Decoder[N]): Decoder[Node[N]] = deriveDecoder[Node[N]]

      implicit def edgeEncoder[E, N](implicit edgeEncoder: Encoder[E]): Encoder[Edge[E]] = graphsEdgeEncoder[E, N]
      implicit def edgeDecoder[E, N](implicit edgeDecoder: Decoder[E]): Decoder[JsonEdge[E]] = deriveDecoder[JsonEdge[E]]

      implicit def defaultGraphEncoder[M, E, N](implicit metaEncoder: Encoder[M],
                                                nodesEncoder: Encoder[Node[N]],
                                                edgesEncoder: Encoder[Edge[E]]): Encoder[JsonGraph[M, E, N]] = new Encoder[JsonGraph[M, E, N]] {
        override def apply(a: JsonGraph[M, E, N]): Json = {
          val fields = new ListBuffer[(String, Json)]
          fields.append("nodes" -> a.graph.nodes.asJson)
          fields.append("edges" -> a.graph.edges.asJson)

          a.meta.map(metaEncoder.apply)
            .filter(_.isNull.unary_!)
            .foreach(metaJson => fields.append("meta" -> metaJson))

          Json.fromFields(fields)
        }
      }

      implicit def defaultGraphDecoder[M, E, N](implicit metaDecoder: Decoder[M],
                                                nodesDecoder: Decoder[Node[N]],
                                                jsonEdgeDecoder: Decoder[JsonEdge[E]]): Decoder[JsonGraph[M, E, N]] = new Decoder[JsonGraph[M, E, N]] {
        override def apply(c: HCursor): Result[JsonGraph[M, E, N]] = for {
          meta <- c.downField("meta").as[Option[M]]
          nodes <- c
            .downField("nodes")
            .as[List[Node[N]]]
            .map(_.iterator.map(node => (node.id, node)).toMap) // convert to map to look up edge references
          jsonEdges <- c.downField("edges").as[Option[List[JsonEdge[E]]]]
        } yield JsonGraph(Graph.fromNodes(nodes).withEdges(toEdges(jsonEdges.getOrElse(List.empty), nodes)), meta)
      }
    }

    private def toEdges[E, N](jsonEdges: List[JsonEdge[E]], nodes: Map[String, Node[N]]): List[Edge[E]] = jsonEdges.flatMap { jsonEdge =>
      for {
        from <- nodes.get(jsonEdge.from)
        to <- nodes.get(jsonEdge.to)
      } yield Edge.of(jsonEdge.value, from.id, to.id)
    }

    private def graphsEdgeEncoder[E, N](implicit edgeEncoder: Encoder[E]): Encoder[Edge[E]] = new Encoder[Edge[E]] {
      override def apply(a: Edge[E]): Json = {
        val encodedValue = edgeEncoder(a.value)
        val fields = new ListBuffer[(String, Json)]
        fields.append("id" -> Json.fromString(a.id))
        fields.append("from" -> Json.fromString(a.from))
        fields.append("to" -> Json.fromString(a.to))
        fields.append("value" -> encodedValue)

        Json.fromFields(fields)
      }
    }

    /**
     * format that uses the node values instead of the wrapper
     */
    object embedded {
      import io.circe.generic.semiauto._

      implicit def nodeEncoder[N](implicit nodeEncoder: Encoder[N]): Encoder[Node[N]] = deriveEncoder[Node[N]]
      implicit def nodeDecoder[N](implicit nodeDecoder: Decoder[N]): Decoder[Node[N]] = deriveDecoder[Node[N]]

      implicit def edgeEncoder[E, N](implicit edgeEncoder: Encoder[E]): Encoder[Edge[E]] = graphsEdgeEncoder[E, N]
      implicit def edgeDecoder[E, N](implicit edgeDecoder: Decoder[E]): Decoder[JsonEdge[E]] = deriveDecoder[JsonEdge[E]]

      implicit def embeddedGraphEncoder[M, E, N](implicit metaEncoder: Encoder[M],
                                                nodesEncoder: Encoder[N],
                                                edgesEncoder: Encoder[Edge[E]]): Encoder[JsonGraph[M, E, N]] = new Encoder[JsonGraph[M, E, N]] {
        override def apply(a: JsonGraph[M, E, N]): Json = Json.obj(
          "meta" -> a.meta.map(metaEncoder.apply).asJson,
          "nodes" -> a.graph.nodes.map(_.value).asJson,
          "edges" -> a.graph.edges.asJson.dropNullValues
        ).asJson
      }

      implicit def embeddedGraphDecoder[M, E, N](implicit metaDecoder: Decoder[M],
                                                nodesDecoder: Decoder[N],
                                                edgeDecoder: Decoder[E],
                                                edgesDecoder: Decoder[JsonEdge[E]],
                                                nodeId: Identifiable[N]): Decoder[JsonGraph[M, E, N]] = new Decoder[JsonGraph[M, E, N]] {
        override def apply(c: HCursor): Result[JsonGraph[M, E, N]] = for {
          meta <- c.downField("meta").as[Option[M]]
          nodes <- c
            .downField("nodes")
            .as[List[N]]
            .map(_.iterator.map(node => (nodeId(node), Node(nodeId(node), node))).toMap)
          jsonEdges <- c.downField("edges").as[Option[List[JsonEdge[E]]]]
        } yield JsonGraph(Graph.fromNodes(nodes).withEdges(toEdges(jsonEdges.getOrElse(List.empty), nodes)), meta)
      }
    }
  }
}
