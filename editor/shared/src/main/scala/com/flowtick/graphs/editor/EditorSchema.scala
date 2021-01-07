package com.flowtick.graphs.editor

import com.flowtick.graphs.json.schema.Schema

trait EditorSchemaLike {
  def schemas: List[Schema[EditorSchemaHints]]

  def merge(other: List[Schema[EditorSchemaHints]]): EditorSchemaLike

  lazy val definitions: Map[String, Schema[EditorSchemaHints]] =
    schemas.view.flatMap(_.definitionsCompat).foldRight(Map.empty[String, Schema[EditorSchemaHints]])(_ ++ _)
}

final case class EditorSchemas(schemas: List[Schema[EditorSchemaHints]]) extends EditorSchemaLike {
  override def merge(other: List[Schema[EditorSchemaHints]]): EditorSchemaLike =
    copy(schemas ++ other.filterNot(_.$id.exists(schemas.flatMap(_.$id).contains)))
}
