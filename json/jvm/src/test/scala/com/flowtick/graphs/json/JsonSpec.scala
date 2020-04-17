package com.flowtick.graphs.json

import com.flowtick.graphs.Graph
import com.flowtick.graphs.defaults._
import com.flowtick.graphs.defaults.id.string
import io.circe
import io.circe.Json
import org.scalatest.{ FlatSpec, Matchers }

class JsonSpec extends FlatSpec with Matchers {
  val numberGraph: Graph[Unit, Unit, Int] = Graph.fromEdges(Set(1 --> 2)).withNode(3)

  "JSON support" should "encode and decode int graph" in {
    val graphJson: Json = ToJson(numberGraph)
    val parsed: Either[circe.Error, Graph[Unit, Unit, Int]] = FromJson[Unit, Unit, Int](graphJson.noSpaces)

    parsed should be(Right(numberGraph))
  }

  it should "parse json without meta" in {
    val numberGraphJson = s"""
       |{
       | "nodes": { "1": 1, "2": 2, "3": 3 },
       | "edges": [
       |   { "id": "1-2", "source": "1", "target": "2", "value": null}
       | ]
       |}
       |""".stripMargin

    val parsed = FromJson[Option[Unit], Unit, Int](numberGraphJson)
    parsed should be(Right(numberGraph.withMeta(None)))
  }

  it should "parse unit graph" in {
    val emptyGraph = s"""
                        |{
                        | "meta": null,
                        | "nodes": {},
                        | "edges": []
                        |}
                        |""".stripMargin

    val parsed = FromJson[Unit, Unit, Int](emptyGraph)
    parsed should be(Right(Graph.unit))
  }

  it should "parse json with meta" in {
    val emptyGraph = s"""
                        |{
                        | "meta" : { "id": "test" },
                        | "nodes": {},
                        | "edges": []
                        |}
                        |""".stripMargin

    val parsed = FromJson[Map[String, String], Unit, Int](emptyGraph)
    parsed should be(Right(Graph.empty(Map("id" -> "test"))))
  }
}
