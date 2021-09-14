package com.flowtick.graphs.json

import cats.data.Validated.Valid
import com.flowtick.graphs.json.schema.Schema
import com.flowtick.graphs.{Graph, Node}
import io.circe.Json
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class JsonExporterSpec extends AnyFlatSpec with Matchers {
  import com.flowtick.graphs.defaults._

  type EdgeType = Unit
  type SchemaType = Unit

  val numberGraph: Graph[EdgeType, Int] =
    Graph.fromEdges(Set(1 --> 2)).withNode(Node.of(3))

  "JSON export" should "export graph with schema" in {
    implicit val nodeJson = new JsonWithSchema[Int, SchemaType] {
      override def json(value: Int): Json =
        Json.obj("value" -> Json.fromInt(value))
      override def schema(value: Int): Schema[Unit] =
        if (value == 1) Schema($id = Some("root"))
        else Schema($id = Some("number"))
    }

    implicit val reference = new Reference[Unit] {
      override def name(relation: EdgeType): Option[String] = Some("child")
    }

    val exported = JsonExporter.exportJson[EdgeType, Int, SchemaType](
      numberGraph,
      Schema($id = Some("root"))
    )

    exported should be(
      Valid(
        Json.obj(
          "value" -> Json.fromInt(1),
          "child" -> Json.obj(
            "value" -> Json.fromInt(2)
          )
        )
      )
    )
  }
}
