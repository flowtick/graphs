package com.flowtick.graphs

import io.circe.Decoder.Result
import io.circe.syntax._
import io.circe._

import scala.collection.mutable.ListBuffer

package object json {

  object format {
    implicit object default {
      implicit val unitAsNull: Codec[Unit] = new Codec[Unit] {
        override def apply(a: Unit): Json = Json.Null
        override def apply(c: HCursor): Result[Unit] = Right(())
      }

      import io.circe.generic.semiauto._

      implicit def nodeEncoder[N](implicit
          nodeEncoder: Encoder[N]
      ): Encoder[Node[N]] = deriveEncoder[Node[N]]
      implicit def nodeDecoder[N](implicit
          nodeDecoder: Decoder[N]
      ): Decoder[Node[N]] = deriveDecoder[Node[N]]

      implicit def edgeEncoder[E, N](implicit
          edgeEncoder: Encoder[E]
      ): Encoder[Edge[E]] = graphsEdgeEncoder[E, N]
      implicit def edgeDecoder[E, N](implicit
          edgeDecoder: Decoder[E]
      ): Decoder[Edge[E]] = deriveDecoder[Edge[E]]

      implicit def defaultGraphEncoder[E, N](implicit
          nodesEncoder: Encoder[Node[N]],
          edgesEncoder: Encoder[Edge[E]]
      ): Encoder[Graph[E, N]] = new Encoder[Graph[E, N]] {
        override def apply(a: Graph[E, N]): Json = {
          val fields = new ListBuffer[(String, Json)]
          fields.append("nodes" -> a.nodes.asJson)
          fields.append("edges" -> a.edges.asJson)
          Json.fromFields(fields)
        }
      }

      implicit def defaultGraphDecoder[E, N](implicit
          nodesDecoder: Decoder[Node[N]],
          edgeDecoder: Decoder[Edge[E]],
          nodeId: Identifiable[N]
      ): Decoder[Graph[E, N]] = new Decoder[Graph[E, N]] {
        override def apply(c: HCursor): Result[Graph[E, N]] = for {
          nodes <- c
            .downField("nodes")
            .as[List[Node[N]]]
          edges <- c.downField("edges").as[Option[List[Edge[E]]]]
        } yield Graph
          .empty[E, N]
          .withNodes(nodes)
          .withEdges(edges.getOrElse(List.empty))
      }
    }

    private def graphsEdgeEncoder[E, N](implicit
        edgeEncoder: Encoder[E]
    ): Encoder[Edge[E]] = new Encoder[Edge[E]] {
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

    /** format that uses the node values instead of the wrapper
      */
    object embedded {
      import io.circe.generic.semiauto._

      implicit def nodeEncoder[N](implicit
          nodeEncoder: Encoder[N]
      ): Encoder[Node[N]] = deriveEncoder[Node[N]]
      implicit def nodeDecoder[N](implicit
          nodeDecoder: Decoder[N]
      ): Decoder[Node[N]] = deriveDecoder[Node[N]]

      implicit def edgeEncoder[E, N](implicit
          edgeEncoder: Encoder[E]
      ): Encoder[Edge[E]] = graphsEdgeEncoder[E, N]
      implicit def edgeDecoder[E, N](implicit
          edgeDecoder: Decoder[E]
      ): Decoder[Edge[E]] = deriveDecoder[Edge[E]]

      implicit def embeddedGraphEncoder[E, N](implicit
          nodesEncoder: Encoder[N],
          edgesEncoder: Encoder[Edge[E]]
      ): Encoder[Graph[E, N]] = new Encoder[Graph[E, N]] {
        override def apply(a: Graph[E, N]): Json = Json
          .obj(
            "nodes" -> a.nodes.map(_.value).asJson,
            "edges" -> a.edges.asJson.dropNullValues
          )
          .asJson
      }

      implicit def embeddedGraphDecoder[E, N](implicit
          nodesDecoder: Decoder[N],
          edgeDecoder: Decoder[E],
          edgesDecoder: Decoder[Edge[E]],
          nodeId: Identifiable[N]
      ): Decoder[Graph[E, N]] = new Decoder[Graph[E, N]] {
        override def apply(c: HCursor): Result[Graph[E, N]] = for {
          nodes <- c.downField("nodes").as[List[N]]
          edges <- c.downField("edges").as[Option[List[Edge[E]]]]
        } yield Graph
          .empty[E, N]
          .addNodes(nodes)
          .withEdges(edges.getOrElse(List.empty))
      }
    }
  }
}
