package com.flowtick.graphs.editor

import com.flowtick.graphs.json.schema.Schema
import com.flowtick.graphs.style.StyleSheet
import io.circe
import io.circe.Decoder.Result
import io.circe.generic.semiauto._
import io.circe.{Codec, Decoder, HCursor}

final case class EditorConfiguration(palettes: Option[List[Palette]] = None) {
  lazy val styleSheets: List[StyleSheet] = palettes.getOrElse(List.empty).map(_.styleSheet)
  lazy val schemas: List[Schema[EditorSchemaHints]] = palettes.getOrElse(List.empty).map(_.schema)
}

object EditorConfiguration {
  implicit val schemaHintsCodec: Codec[EditorSchemaHints] = deriveCodec[EditorSchemaHints]

  implicit val configDecoder: Decoder[EditorConfiguration] = new Decoder[EditorConfiguration] {
    override def apply(c: HCursor): Result[EditorConfiguration] = for {
      palettes <- {
        import io.circe.generic.auto._
        implicit val schemaDecoder: Decoder[Schema[EditorSchemaHints]] = com.flowtick.graphs.json.schema.JsonSchema.jsonSchemaDecoder
        c.downField("palettes").as[Option[List[Palette]]]
      }
    } yield EditorConfiguration(palettes)
  }

  def decode(schemasJson: String): Either[circe.Error, EditorConfiguration] = {
    io.circe.parser.decode[EditorConfiguration](schemasJson)
  }
}