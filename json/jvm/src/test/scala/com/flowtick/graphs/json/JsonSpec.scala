package com.flowtick.graphs.json

import com.flowtick.graphs.defaults._
import com.flowtick.graphs.{Edge, Graph, Node}
import io.circe._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class JsonSpec extends AnyFlatSpec with Matchers {
  val numberGraph: Graph[Unit, Int] = Graph.fromEdges(Set(1 --> 2)).withNode(Node.of(3))

  "JSON support" should "encode and decode int graph" in {
    import com.flowtick.graphs.json.format.default._

    val graphJson: Json = ToJson[Unit, Unit, Int](numberGraph, None)
    val parsed: Either[Error, JsonGraph[Unit, Unit, Int]] = FromJson[Unit, Unit, Int](graphJson.noSpaces)

    println(graphJson.spaces2)

    parsed.map(_.graph) should be(Right(numberGraph))
  }

  it should "parse json without meta" in {
    val numberGraphJson = s"""
                             |{
                             |  "nodes" : [
                             |    {
                             |      "id" : "1",
                             |      "value" : 1
                             |    },
                             |    {
                             |      "id" : "2",
                             |      "value" : 2
                             |    },
                             |    {
                             |      "id" : "3",
                             |      "value" : 3
                             |    }
                             |  ],
                             |  "edges" : [
                             |    {
                             |      "id" : "1-2",
                             |      "value" : null,
                             |      "from" : "1",
                             |      "to" : "2"
                             |    }
                             |  ]
                             |}
                             |
                             |""".stripMargin

    import com.flowtick.graphs.json.format.default._

    val parsed = FromJson[Unit, Unit, Int](numberGraphJson)
    parsed.map(_.graph) should be(Right(numberGraph))
  }

  it should "parse empty graph" in {
    val emptyGraph = s"""
                        |{
                        | "meta": null,
                        | "nodes": [],
                        | "edges": []
                        |}
                        |""".stripMargin

    import com.flowtick.graphs.json.format.default._

    val parsed = FromJson[Unit, Unit, Int](emptyGraph)
    parsed.map(_.graph) should be(Right(Graph.empty))
  }

  it should "treat unit as null if option is imported" in {
    import com.flowtick.graphs.json.format.default._

    val parsed = ToJson[Unit, Unit, Int](Graph.empty.withEdge(Edge.of((), Node("1", 1), Node("2", 2))))

    val edgesJson = parsed.hcursor
      .downField("edges")
      .as[List[Json]]

    edgesJson
      .getOrElse(fail("edges cant be parsed"))
      .headOption.flatMap(_.asObject).flatMap(_("value")) should be(Some(Json.Null))
  }

  it should "parse json with meta" in {
    val emptyGraph = s"""
                        |{
                        | "meta" : { "id": "test" },
                        | "nodes": [],
                        | "edges": []
                        |}
                        |""".stripMargin
    import com.flowtick.graphs.json.format.default._

    val parsed = FromJson[Map[String, String], Unit, Int](emptyGraph)
    parsed should be(Right(JsonGraph(Graph.empty, Some(Map("id" -> "test")))))
  }


  it should "parse embedded graph" in {
    val emptyGraph = s"""
                        |{
                        | "nodes": [1,2],
                        | "edges": [{ "id": "1-2", "from": "1", "to": "2"}]
                        |}
                        |""".stripMargin
    import com.flowtick.graphs.json.format.embedded._

    val parsed = FromJson[Unit, Option[Unit], Int](emptyGraph)
    parsed should be(Right(JsonGraph(Graph.empty.addEdge(None, 1, 2))))
  }

  it should "create embedded graph json" in {
    val expectedGraph = s"""
                        |{
                        | "meta": null,
                        | "nodes": [1,2],
                        | "edges": [{ "id": "1-2", "from": "1", "to": "2", "value": null}]
                        |}
                        |""".stripMargin
    import com.flowtick.graphs.json.format.embedded._

    val json = ToJson[Unit, Option[Unit], Int](Graph.empty.addEdge(None, 1, 2))

    io.circe.parser.decode[Json](expectedGraph) match {
      case Right(expectedJson) => expectedJson.spaces2 should be(json.spaces2)
      case Left(error) => fail(error)
    }
  }

}
