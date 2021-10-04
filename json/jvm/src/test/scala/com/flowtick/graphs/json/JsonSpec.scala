package com.flowtick.graphs.json

import com.flowtick.graphs.defaults._
import com.flowtick.graphs.{Graph, Node}
import io.circe._
import io.circe.syntax._
import io.circe.parser._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class JsonSpec extends AnyFlatSpec with Matchers {
  val numberGraph: Graph[Unit, Int] =
    Graph.fromEdges(Set(1 --> 2)).withNode(Node.of(3))

  "JSON support" should "encode and decode int graph" in {
    import com.flowtick.graphs.json.format.default._

    val graphJson: Json = numberGraph.asJson
    val parsed: Either[Error, Graph[Unit, Int]] = decode[Graph[Unit, Int]](graphJson.noSpaces)

    parsed should be(Right(numberGraph))
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

    val parsed = decode[Graph[Unit, Int]](numberGraphJson)
    parsed should be(Right(numberGraph))
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

    val parsed = decode[Graph[Unit, Int]](emptyGraph)
    parsed should be(Right(Graph.empty[Unit, Int]))
  }

  it should "treat unit as null if option is imported" in {
    import com.flowtick.graphs.json.format.default._

    val parsed =
      Graph.empty[Unit, Int].withEdgeValue((), Node("1", 1), Node("2", 2)).asJson

    val edgesJson = parsed.hcursor
      .downField("edges")
      .as[List[Json]]

    edgesJson
      .getOrElse(fail("edges cant be parsed"))
      .headOption
      .flatMap(_.asObject)
      .flatMap(_("value")) should be(Some(Json.Null))
  }

  it should "parse embedded graph" in {
    val emptyGraph = s"""
                        |{
                        | "nodes": [1,2],
                        | "edges": [{ "id": "1-2", "from": "1", "to": "2"}]
                        |}
                        |""".stripMargin
    import com.flowtick.graphs.json.format.embedded._

    val parsed = decode[Graph[Option[Unit], Int]](emptyGraph)
    parsed should be(
      Right(Graph.empty[Option[Unit], Int].withEdgeValue(None, Node.of(1), Node.of(2)))
    )
  }

  it should "create embedded graph json" in {
    val expectedGraph = s"""
                        |{
                        | "nodes": [1,2],
                        | "edges": [{ "id": "1-2", "from": "1", "to": "2", "value": null}]
                        |}
                        |""".stripMargin
    import com.flowtick.graphs.json.format.embedded._

    val json = Graph.empty[Option[Unit], Int].withEdgeValue(None, Node.of(1), Node.of(2)).asJson

    io.circe.parser.decode[Json](expectedGraph) match {
      case Right(expectedJson) => expectedJson.spaces2 should be(json.spaces2)
      case Left(error)         => fail(error)
    }
  }

}
