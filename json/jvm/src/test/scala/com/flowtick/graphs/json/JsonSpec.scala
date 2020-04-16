package com.flowtick.graphs.json

import com.flowtick.graphs.Graph
import com.flowtick.graphs.defaults._
import com.flowtick.graphs.defaults.id.string
import io.circe
import io.circe.Json
import org.scalatest.{ FlatSpec, Matchers }

class JsonSpec extends FlatSpec with Matchers {
  "JSON support" should "encode and decode int graph" in {
    val graph: Graph[Unit, Int, Unit] = Graph.fromEdges(Set(1 --> 2))
    val graphJson: Json = ToJson(graph)

    val parsed: Either[circe.Error, Graph[Unit, Int, Unit]] = FromJson[Unit, Int, Unit](graphJson.noSpaces)
    parsed should be(Right(graph))
  }

  it should "parse json without meta" in {
    val emptyGraph = s"""
       |{
       | "nodes": {},
       | "edges": []
       |}
       |""".stripMargin

    val parsed = FromJson[Unit, Int, Option[Unit]](emptyGraph)
    parsed should be(Right(Graph.empty(None)))
  }

  it should "parse json with meta" in {
    val emptyGraph = s"""
                        |{
                        | "meta" : { "id": "test" },
                        | "nodes": {},
                        | "edges": []
                        |}
                        |""".stripMargin

    val parsed = FromJson[Unit, Int, Map[String, String]](emptyGraph)
    parsed should be(Right(Graph.empty(Map("id" -> "test"))))
  }
}
