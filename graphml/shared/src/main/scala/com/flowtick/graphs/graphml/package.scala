package com.flowtick.graphs

import cats._
import cats.data._
import cats.data.Validated._
import cats.syntax._
import cats.implicits._
import shapeless._

import scala.xml.NodeSeq

package object graphml {

  trait Serializer[T] {
    def serialize(value: T): NodeSeq
  }

  trait Deserializer[T] {
    def deserialize(from: NodeSeq): ValidatedNel[Throwable, T]
  }

  trait Datatype[T] extends Serializer[T] with Deserializer[T]

  object Datatype extends ProductTypeClassCompanion[Datatype] {
    object typeClass extends ProductTypeClass[Datatype] {
      def emptyProduct: Datatype[HNil] = new Datatype[HNil] {
        def serialize(value: HNil): NodeSeq = Nil
        def deserialize(from: NodeSeq) = Valid(HNil)
      }

      def product[H, T <: HList](
        dh: Datatype[H],
        dt: Datatype[T]): Datatype[H :: T] = new Datatype[H :: T] {
        def serialize(value: H :: T): NodeSeq =
          dh.serialize(value.head) ++ dt.serialize(value.tail)

        def deserialize(from: NodeSeq): ValidatedNel[Throwable, H :: T] =
          (
            from.headOption.map(dh.deserialize).getOrElse(invalidNel[Throwable, H](new IllegalArgumentException("can't serialize empty node sequence"))),
            dt.deserialize(from.drop(1))).mapN(_ :: _)
      }

      def project[F, G](
        instance: => Datatype[G],
        to: F => G,
        from: G => F): Datatype[F] = new Datatype[F] {
        def serialize(value: F): NodeSeq = instance.serialize(to(value))
        def deserialize(nodes: NodeSeq): Validated[NonEmptyList[Throwable], F] = instance.deserialize(nodes).map(from)
      }
    }
  }

  implicit object DatatypeString extends Datatype[String] {
    def serialize(value: String) = <s>{ value }</s>
    def deserialize(from: NodeSeq) = from match {
      case <s>{ value }</s> => value.text.validNel
      case _ => new RuntimeException("Bad string XML").invalidNel
    }
  }

  def nodeProperty(id: String, value: Any, typeHint: Option[String] = None) =
    GraphMLProperty(GraphMLKey(id, targetHint = Some("node"), typeHint = typeHint), value)

  def graphMlNode[N](id: String, value: N, properties: GraphMLProperty*): GraphMLNode[N] =
    GraphMLNode(id, value, None, properties.map(prop => (prop.key.id, prop)).toMap)

  implicit def identifiable[N]: Identifiable[GraphMLNode[N]] = new Identifiable[GraphMLNode[N]] {
    override def id(node: GraphMLNode[N]): String = node.id
  }

}
