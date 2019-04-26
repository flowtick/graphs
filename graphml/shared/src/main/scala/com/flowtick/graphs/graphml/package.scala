package com.flowtick.graphs

import cats.data.Validated._
import cats.data._
import cats.implicits._
import shapeless._
import shapeless.ops.record.Keys
import shapeless.ops.traversable.FromTraversable
import shapeless.ops.traversable.FromTraversable._

import scala.collection.GenTraversable
import scala.xml.NodeSeq

package object graphml {

  trait Serializer[T] {
    def serialize(value: T): NodeSeq
    def keys: Seq[GraphMLKey] = Seq.empty
  }

  trait Deserializer[T] {
    def deserialize(from: NodeSeq): ValidatedNel[Throwable, T]
  }

  trait Datatype[T] extends Serializer[T] with Deserializer[T]

  object Datatype extends LabelledProductTypeClassCompanion[Datatype] {
    object typeClass extends LabelledProductTypeClass[Datatype] {
      def emptyProduct: Datatype[HNil] = new Datatype[HNil] {
        def serialize(value: HNil): NodeSeq = Nil
        def deserialize(from: NodeSeq) = Valid(HNil)
      }

      def product[H, T <: HList](
        name: String,
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
    def serialize(value: String): NodeSeq = <value>{ value }</value>

    def deserialize(from: NodeSeq): Validated[NonEmptyList[Throwable], String] = from match {
      case <value>{ value }</value> => value.text.validNel
      case _ => new RuntimeException("Bad string XML").invalidNel
    }
  }

  implicit object DatatypeUnit extends Datatype[Unit] {
    def serialize(value: Unit): NodeSeq = <!-- unit -->
    def deserialize(from: NodeSeq): Validated[NonEmptyList[Throwable], Unit] = valid()
  }

  implicit def optionalDataType[T](implicit dataType: Datatype[T]): Datatype[Option[T]] = new Datatype[Option[T]] {
    def serialize(value: Option[T]): NodeSeq = value match {
      case None => <!-- empty optional -->
      case Some(actualValue) => dataType.serialize(actualValue)
    }

    def deserialize(from: NodeSeq): Validated[NonEmptyList[Throwable], Option[T]] = dataType.deserialize(from).map(Option(_))
  }

  implicit def doubleDataType(implicit stringDataType: Datatype[String]): Datatype[Double] = new Datatype[Double] {
    def serialize(value: Double): NodeSeq = stringDataType.serialize(value.toString)
    def deserialize(from: NodeSeq): Validated[NonEmptyList[Throwable], Double] = stringDataType.deserialize(from).map(stringValue => stringValue.toDouble)
  }

  implicit def graphMLNodeDataType[T, Repr <: HList](implicit genericValue: shapeless.LabelledGeneric.Aux[T, Repr], keys: Keys[Repr]): GraphMLNodeDatatype[T, Repr] = new GraphMLNodeDatatype[T, Repr]()

  def nodeProperty(id: String, value: Any, typeHint: Option[String] = None) =
    GraphMLProperty(GraphMLKey(id, targetHint = Some("node"), typeHint = typeHint), value)

  def ml[N](value: N, id: Option[String] = None, properties: Seq[GraphMLProperty] = Seq.empty): GraphMLNode[N] =
    GraphMLNode(id.getOrElse(value.toString), value, None, properties)

  implicit class EdgeBuilder[X](node: GraphMLNode[X]) {
    def -->[V](value: V, to: GraphMLNode[X]): Edge[GraphMLEdge[V], GraphMLNode[X]] = Edge[GraphMLEdge[V], GraphMLNode[X]](GraphMLEdge(s"${node.id}-${to.id}", value), node, to)
    def -->[V](to: GraphMLNode[X]): Edge[GraphMLEdge[Unit], GraphMLNode[X]] = Edge[GraphMLEdge[Unit], GraphMLNode[X]](GraphMLEdge(s"${node.id}-${to.id}", ()), node, to)
  }

  implicit def graphMLNodeIdentifiable[N]: Identifiable[GraphMLNode[N]] = new Identifiable[GraphMLNode[N]] {
    override def id(node: GraphMLNode[N]): String = node.id
  }

  implicit def graphMLEdgeLabel[V, N]: Labeled[Edge[GraphMLEdge[V], GraphMLNode[N]], String] = new Labeled[Edge[GraphMLEdge[V], GraphMLNode[N]], String] {
    override def label(edge: Edge[GraphMLEdge[V], GraphMLNode[N]]): Option[String] = Some(edge.value.id)
  }

}
