package com.flowtick.graphs.json

import cats.data.Validated
import cats.data.Validated._
import com.flowtick.graphs.{Graph, Node}
import com.flowtick.graphs.json.schema.Schema
import io.circe.Json

trait JsonWithSchema[T, S] {
  def json(value: T): Json
  def schema(value: T): Schema[S]
}

trait Reference[E] {
  def name(relation: E): Option[String]
  def transform: Json => Json = identity
}

object JsonExporter {
  def exportNode[E, N, S](node: Node[N], graph: Graph[E, N])(implicit nodeJson: JsonWithSchema[N, S], reference: Reference[E]): Validated[IllegalArgumentException, Json] = {
    val json = nodeJson.json(node.value)

    graph.outgoing(node.id).foldLeft[Validated[IllegalArgumentException, Json]](Valid(json)) {
      case (json, edge) => reference.name(edge.value) match {
        case Some(field) =>
          // FIXME: not stack-safe
          graph
            .findNode(edge.to)
            .map(exportNode(_, graph))
            .getOrElse(json)
            .andThen { referencedJson =>
              json.map(_.mapObject(obj => obj.add(field, reference.transform(referencedJson))))
            }

        case None => json
      }
    }
  }

  def exportJson[E: Reference, N, S](graph: Graph[E, N], schema: Schema[S])(implicit nodeJson: JsonWithSchema[N, S]): Validated[IllegalArgumentException, Json] = {
    val exported = for {
      rootNode <- schema.$id.flatMap(rootSchemaId => graph.nodes.find(node => nodeJson.schema(node.value).$id.contains(rootSchemaId)))
    } yield exportNode(rootNode, graph)
    exported.getOrElse(Invalid(new IllegalArgumentException(s"could not find root node matching the schema id: ${schema.$id}")))
  }
}
