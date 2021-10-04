package com.flowtick.graphs.editor

import com.flowtick.graphs.{Edge, Graph, Node}
import io.circe.Decoder.Result
import io.circe.{Decoder, Encoder, HCursor, Json, _}

import scala.collection.mutable.ListBuffer
import io.circe.generic.auto._
import com.flowtick.graphs.json.format.default._
import com.flowtick.graphs.layout.{DefaultGeometry, Geometry, GraphLayout, GraphLayoutLike}
import com.flowtick.graphs.style.StyleSheet

object EditorGraphJsonFormat {
  implicit def decodeEitherStringA[A](implicit
      decoderA: Decoder[A],
      decoderB: Decoder[String]
  ): Decoder[Either[String, A]] = decoderB.either(decoderA)

  implicit def encodeEitherStringA[A](implicit
      encoderA: Encoder[A],
      encoderB: Encoder[String]
  ): Encoder[Either[String, A]] = new Encoder[Either[String, A]] {
    override def apply(a: Either[String, A]): Json = a match {
      case Right(a) => encoderA(a)
      case Left(b)  => encoderB(b)
    }
  }

  implicit val geometryCode = new Codec[Geometry] {
    override def apply(a: Geometry): Json = Json.obj(
      "x" -> Json.fromDoubleOrNull(a.x),
      "y" -> Json.fromDoubleOrNull(a.y),
      "height" -> Json.fromDoubleOrNull(a.height),
      "width" -> Json.fromDoubleOrNull(a.width)
    )

    override def apply(c: HCursor): Result[Geometry] = for {
      x <- c.downField("x").as[Double]
      y <- c.downField("y").as[Double]
      width <- c.downField("width").as[Double]
      height <- c.downField("height").as[Double]
    } yield DefaultGeometry(x, y, width, height)
  }

  implicit val defaultEditorGraphEncoder: Encoder[EditorGraph] =
    new Encoder[EditorGraph] {
      import io.circe.syntax._

      override def apply(editorGraph: EditorGraph): Json = {
        val fields = new ListBuffer[(String, Json)]
        fields.append(
          "graph" -> Json.obj(
            "nodes" -> editorGraph.graph.nodes.asJson,
            "edges" -> editorGraph.graph.edges.asJson
          ),
          "styleSheets" -> editorGraph.styleSheets.asJson,
          "layouts" -> editorGraph.layouts
            .flatMap(_.toOption)
            .flatMap(_.toGraphLayouts)
            .asJson,
          "schemas" -> editorGraph.schemas.asJson
        )

        Json.fromFields(fields)
      }
    }

  implicit val defaultEditorGraphDecoder: Decoder[EditorGraph] =
    new Decoder[EditorGraph] {
      override def apply(json: HCursor): Result[EditorGraph] = for {
        nodes <- json
          .downField("graph")
          .downField("nodes")
          .as[List[Node[EditorGraphNode]]]

        edges <- json
          .downField("graph")
          .downField("edges")
          .as[Option[List[Edge[EditorGraphEdge]]]]

        styleSheets <- json
          .downField("styleSheets")
          .as[List[Either[String, StyleSheet]]]
        layouts <- json
          .downField("layouts")
          .as[List[Either[String, GraphLayout]]]
        schemas <- json
          .downField("schemas")
          .as[List[Either[String, EditorModel.EditorSchema]]]
      } yield EditorGraph(
        Graph
          .empty[EditorGraphEdge, EditorGraphNode]
          .withNodes(nodes)
          .withEdges(edges.getOrElse(List.empty)),
        styleSheets,
        layouts,
        schemas
      )
    }
}
