package com.flowtick.graphs.json.schema

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class JsonSchemaSpec extends AnyFlatSpec with Matchers {
  "JSOM Schema" should "be parsed" in {
    val schemaString = s"""
      |{
      |    "$$id": "https://example.net/root.json",
      |    "$$schema": "http://json-schema.org/draft/2019-09/schema#",
      |    "$$defs": {
      |        "node": {
      |            "type": "object",
      |            "properties": {
      |                "text": {
      |                    "title": "Some Text",
      |                    "type": "string",
      |                    "extension": "ext"
      |                },
      |                "someInt": {
      |                    "title": "Some Integer",
      |                    "type": "integer"
      |                },
      |                "someBool": {
      |                    "title": "Some Boolean",
      |                    "type": "boolean"
      |                },
      |                "someFloat": {
      |                    "title": "Some Float",
      |                    "type": "number"
      |                }
      |            }
      |        },
      |        "textOnly": {
      |            "title": "Text",
      |            "type": "string"
      |        }
      |    }
      |}""".stripMargin

    JsonSchema.parse[String](schemaString) match {
      case Right(schemaJson) => succeed
      case Left(error) => fail(error)
    }
  }
}
