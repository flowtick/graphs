package com.flowtick.graphs.json

import com.flowtick.graphs.Graph
import com.flowtick.graphs.defaults._
import com.flowtick.graphs.defaults.id._
import io.circe
import io.circe.Json
import org.scalatest.{ FlatSpec, Matchers }

class JsonSpec extends FlatSpec with Matchers {
  type IntGraph = Graph[Unit, Int, Unit]

  "JSON support" should "encode and decode int graph" in {
    val graph: IntGraph = Graph.fromEdges(Set(n(1) --> n(2)))
    val graphJson: Json = ToJson(graph)

    val parsed: Either[circe.Error, IntGraph] = FromJson[Unit, Int, Unit](graphJson.noSpaces)
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
}
