package com.flowtick.graphs.json.schema

import com.flowtick.graphs.json.schema.JsonSchema.SingleOrList
import io.circe
import io.circe.{Decoder, Encoder, Json}
import io.circe.generic.auto._
import io.circe.syntax._

sealed trait JsonSchemaEnum

final case class Schema[E]($id: Option[String] = None,
                        $schema: Option[String] = None,
                        title: Option[String] = None,
                        description: Option[String] = None,
                        `type`: Option[SingleOrList[String]] = None,
                        required: Option[List[String]] = None,
                        $ref: Option[String] = None,
                        additionalProperties: Option[Either[Boolean, Schema[E]]] = None,
                        enum: Option[
                          Either[
                            List[Boolean],
                            List[Either[Double, String]]]
                        ] = None,
                        properties: Option[Map[String, Schema[E]]] = None,
                        items: Option[SingleOrList[Schema[E]]] = None,
                        definitions: Option[Map[String, Schema[E]]] = None,
                        $defs: Option[Map[String, Schema[E]]] = None,
                        anyOf: Option[List[Schema[E]]] = None,
                        oneOf: Option[List[Schema[E]]] = None,
                        minItems: Option[Int] = None,
                        maxItems: Option[Int] = None,
                        minimum: Option[Int] = None,
                        maximum: Option[Int] = None,
                        minLength: Option[Int] = None,
                        maxLength: Option[Int] = None,
                        pattern: Option[String] = None,
                        extension: Option[E] = None) {
  def definitionsCompat: Option[Map[String, Schema[E]]] = definitions.orElse($defs)
}

object JsonSchema {
  type SingleOrList[A] = Either[List[A], A]

  implicit def decodeSingleOrList[A](implicit
                                        singleDecoder: Decoder[A],
                                        listDecoder: Decoder[List[A]]): Decoder[SingleOrList[A]] = listDecoder.either(singleDecoder)

  implicit def decodeEitherBA[B, A](implicit
                                     decoderA: Decoder[A],
                                     decoderB: Decoder[B]): Decoder[Either[B, A]] = decoderB.either(decoderA)

  implicit def encodeSingleOrList[A](implicit
                                     singleEncoder: Encoder[A],
                                     listEncoder: Encoder[List[A]]): Encoder[SingleOrList[A]] = {
    case Right(single) => singleEncoder(single)
    case Left(list) => listEncoder(list)
  }

  def parse[E](schemaJson: String)(implicit decoder: Decoder[E]): Either[circe.Error, Schema[E]] = io.circe.parser.decode[Schema[E]](schemaJson)

  def toJson[E](schema: Schema[E])(implicit encoder: Encoder[E]): Json = schema.asJson
}
