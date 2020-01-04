package com.flowtick.graphs

import cats.data.Validated._
import cats.data._
import cats.implicits._
import com.flowtick.graphs.graphml.GraphMLDatatype.parseKeys

import xmls.XMLS

import scala.collection.GenTraversable
import scala.reflect.ClassTag
import scala.util.{ Either, Left, Right }
import scala.xml.NodeSeq

package object graphml {
  type GraphMLGraphType[V, N, M] = Graph[GraphMLEdge[V], GraphMLNode[N], GraphMLGraph[M]]

  final case class ValueWithProperties[T](value: T, properties: Seq[GraphMLProperty] = Seq.empty)
  final case class WrappedValue[T](value: T)

  trait Serializer[T] {
    def serialize(value: T): NodeSeq
    def keys: Seq[GraphMLKey] = Seq.empty
  }

  trait Deserializer[T] {
    def deserialize(from: NodeSeq, graphKeys: scala.collection.Map[String, GraphMLKey]): ValidatedNel[Throwable, T]
  }

  trait FromList[T, R] {
    def apply(l: GenTraversable[_]): Option[T]
  }

  trait Datatype[T] extends Serializer[T] with Deserializer[T]

  object generic {
    import shapeless._
    import shapeless.ops.record.Keys
    import shapeless.ops.traversable.FromTraversable

    /**
     * there is no default Typeable for LabelledGeneric (more precisely the KeyTags),
     * so we need to fallback to the generic representation of T to create the value from a list.
     *
     * @param generic
     * @param typeable
     * @param fromTraversable
     * @tparam T
     * @tparam Repr
     */
    class FromListGeneric[T, Repr <: HList](implicit
      generic: Generic.Aux[T, Repr],
      typeable: Typeable[Repr],
      fromTraversable: FromTraversable[Repr]) extends FromList[T, Repr] {
      import shapeless.syntax.std.traversable._

      def apply(l: GenTraversable[_]): Option[T] = {
        l.toHList[Repr].map(generic.from)
      }
    }

    object Datatype extends LabelledProductTypeClassCompanion[Datatype] {
      def apply[T](implicit dt: Datatype[T]): Datatype[T] = dt

      object typeClass extends LabelledProductTypeClass[Datatype] {
        def emptyProduct: Datatype[HNil] = new Datatype[HNil] {
          def serialize(value: HNil): NodeSeq = Nil
          def deserialize(from: NodeSeq, graphKeys: scala.collection.Map[String, GraphMLKey]) = Valid(HNil)
        }

        def product[H, T <: HList](
          name: String,
          dh: Datatype[H],
          dt: Datatype[T]): Datatype[H :: T] = new Datatype[H :: T] {
          def serialize(value: H :: T): NodeSeq =
            dh.serialize(value.head) ++ dt.serialize(value.tail)

          def deserialize(from: NodeSeq, graphKeys: scala.collection.Map[String, GraphMLKey]): ValidatedNel[Throwable, H :: T] =
            (
              from.headOption.map(dh.deserialize(_, graphKeys)).getOrElse(invalidNel[Throwable, H](new IllegalArgumentException("can't serialize empty node sequence"))),
              dt.deserialize(from.drop(1), graphKeys)).mapN(_ :: _)
        }

        def project[F, G](
          instance: => Datatype[G],
          to: F => G,
          from: G => F): Datatype[F] = new Datatype[F] {
          def serialize(value: F): NodeSeq = instance.serialize(to(value))
          def deserialize(nodes: NodeSeq, graphKeys: scala.collection.Map[String, GraphMLKey]): Validated[NonEmptyList[Throwable], F] = instance.deserialize(nodes, graphKeys).map(from)
        }
      }
    }

    implicit def genericFromList[T, Repr <: HList](implicit generic: Generic.Aux[T, Repr], typeable: Typeable[Repr], fromTraversable: FromTraversable[Repr]): FromListGeneric[T, Repr] = new FromListGeneric[T, Repr]
    implicit def stringFromList(implicit generic: Generic.Aux[String, String :: HNil], typeable: Typeable[String :: HNil], fromTraversable: FromTraversable[String :: HNil]): FromListGeneric[String, String :: HNil] = new FromListGeneric[String, String :: HNil]

    implicit def graphMLNodeDataTypeGeneric[T, Repr <: HList, FromRepr <: HList](implicit classTag: ClassTag[T], genericValue: shapeless.LabelledGeneric.Aux[T, Repr], keys: Keys[Repr], fromList: FromListGeneric[T, FromRepr]): Datatype[GraphMLNode[T]] = new GraphMLNodeDatatype[T, Repr, FromRepr]
    implicit def graphMLEdgeDataTypeGeneric[T, Repr <: HList, FromRepr <: HList](implicit classTag: ClassTag[T], genericValue: shapeless.LabelledGeneric.Aux[T, Repr], keys: Keys[Repr], fromList: FromListGeneric[T, FromRepr]): Datatype[GraphMLEdge[T]] = new GraphMLEdgeDatatype[T, Repr, FromRepr]

    implicit val graphMLUnitNodeDataType: Datatype[GraphMLNode[Unit]] = new GraphMLNodeDatatype[Unit, HNil, HNil]
    implicit val graphMLUnitEdgeDataType: Datatype[GraphMLEdge[Unit]] = new GraphMLEdgeDatatype[Unit, HNil, HNil]

    implicit val graphMLStringNodeDataType: Datatype[GraphMLNode[String]] = new WrappedNodeDatatype[String]
    implicit val graphMLStringEdgeDataType: Datatype[GraphMLEdge[String]] = new WrappedEdgeDatatype[String]

    implicit val graphMLIntNodeDataType: Datatype[GraphMLNode[Int]] = new WrappedNodeDatatype[Int]
    implicit val graphMLIntEdgeDataType: Datatype[GraphMLEdge[Int]] = new WrappedEdgeDatatype[Int]

    implicit val graphMLDoubleNodeDataType: Datatype[GraphMLNode[Double]] = new WrappedNodeDatatype[Double]
    implicit val graphMLDoubleEdgeDataType: Datatype[GraphMLEdge[Double]] = new WrappedEdgeDatatype[Double]
  }

  implicit object DatatypeString extends Datatype[String] {
    def serialize(value: String): NodeSeq = <value>{ value }</value>

    def deserialize(from: NodeSeq, graphKeys: scala.collection.Map[String, GraphMLKey]): Validated[NonEmptyList[Throwable], String] = from match {
      case <value>{ value }</value> => value.text.validNel
      case _ => new RuntimeException("Bad string XML").invalidNel
    }
  }

  implicit object DatatypeUnit extends Datatype[Unit] {
    def serialize(value: Unit): NodeSeq = NodeSeq.Empty
    def deserialize(from: NodeSeq, graphKeys: scala.collection.Map[String, GraphMLKey]): Validated[NonEmptyList[Throwable], Unit] = valid()
  }

  implicit def optionalDataType[T](implicit dataType: Datatype[T]): Datatype[Option[T]] = new Datatype[Option[T]] {
    def serialize(value: Option[T]): NodeSeq = value match {
      case None => <!-- empty optional -->
      case Some(actualValue) => dataType.serialize(actualValue)
    }

    def deserialize(from: NodeSeq, graphKeys: scala.collection.Map[String, GraphMLKey]): Validated[NonEmptyList[Throwable], Option[T]] = dataType.deserialize(from, graphKeys).map(Option(_))
  }

  implicit def doubleDataType(implicit stringDataType: Datatype[String]): Datatype[Double] = new Datatype[Double] {
    def serialize(value: Double): NodeSeq = stringDataType.serialize(value.toString)
    def deserialize(from: NodeSeq, graphKeys: scala.collection.Map[String, GraphMLKey]): Validated[NonEmptyList[Throwable], Double] = stringDataType.deserialize(from, graphKeys: scala.collection.Map[String, GraphMLKey]).map(stringValue => stringValue.toDouble)
  }

  implicit val graphMLUnitMetaDataType: Datatype[GraphMLGraph[Unit]] = new GraphMLMetaDatatype[Unit]

  class WrappedNodeDatatype[T](implicit wrapped: Datatype[GraphMLNode[WrappedValue[T]]]) extends Datatype[GraphMLNode[T]] {
    override def keys: Seq[GraphMLKey] = wrapped.keys

    override def serialize(node: GraphMLNode[T]): NodeSeq = wrapped.serialize(
      GraphMLNode[WrappedValue[T]](
        node.id,
        WrappedValue(node.value),
        node.label,
        node.properties,
        node.shape,
        node.geometry))

    override def deserialize(from: NodeSeq, graphKeys: collection.Map[String, GraphMLKey]): ValidatedNel[Throwable, GraphMLNode[T]] =
      wrapped.deserialize(from, graphKeys).map(node => GraphMLNode[T](node.id, node.value.value, node.label, node.properties, node.shape, node.geometry))
  }

  class WrappedEdgeDatatype[T](implicit wrapped: Datatype[GraphMLEdge[WrappedValue[T]]]) extends Datatype[GraphMLEdge[T]] {
    override def keys: Seq[GraphMLKey] = wrapped.keys

    override def serialize(value: GraphMLEdge[T]): NodeSeq = wrapped.serialize(
      GraphMLEdge(value.id, WrappedValue(value.value), value.source, value.target, value.label, value.properties))

    override def deserialize(from: NodeSeq, graphKeys: collection.Map[String, GraphMLKey]): ValidatedNel[Throwable, GraphMLEdge[T]] =
      wrapped.deserialize(from, graphKeys).map(edge => GraphMLEdge(edge.id, edge.value.value, edge.source, edge.target, edge.label, edge.properties))
  }

  implicit def graphMLDataType[V, N, M](implicit
    identifiable: Identifiable[GraphMLNode[N]],
    edgeLabel: Labeled[Edge[GraphMLEdge[V], GraphMLNode[N]], String],
    nodeDataType: Datatype[GraphMLNode[N]],
    edgeDataType: Datatype[GraphMLEdge[V]],
    metaDataType: Datatype[GraphMLGraph[M]]): Datatype[GraphMLGraphType[V, N, M]] = new GraphMLDatatype[V, N, M]

  def ml[N](nodeValue: N, id: Option[String] = None, properties: Seq[GraphMLProperty] = Seq.empty): GraphMLNode[N] =
    GraphMLNode(id.getOrElse(nodeValue.toString), nodeValue, None, properties)

  implicit class GraphMLEdgeBuilder[X](node: GraphMLNode[X]) {
    def -->[V](value: V, to: GraphMLNode[X]): Edge[GraphMLEdge[V], GraphMLNode[X]] = Edge[GraphMLEdge[V], GraphMLNode[X]](GraphMLEdge(s"${node.id}-${to.id}", value, Some(node.id), Some(to.id)), node, to)
    def -->(to: GraphMLNode[X]): Edge[GraphMLEdge[Unit], GraphMLNode[X]] = Edge[GraphMLEdge[Unit], GraphMLNode[X]](GraphMLEdge(s"${node.id}-${to.id}", (), Some(node.id), Some(to.id)), node, to)
  }

  implicit def graphMLNodeIdentifiable[N]: Identifiable[GraphMLNode[N]] = new Identifiable[GraphMLNode[N]] {
    override def id(node: GraphMLNode[N]): String = node.id
  }

  implicit def graphMLEdgeLabel[V, N]: Labeled[Edge[GraphMLEdge[V], GraphMLNode[N]], String] = new Labeled[Edge[GraphMLEdge[V], GraphMLNode[N]], String] {
    override def label(edge: Edge[GraphMLEdge[V], GraphMLNode[N]]): Option[String] = Some(edge.value.id)
  }

  implicit class GraphMLOps[V, N, M](graph: GraphMLGraphType[V, N, M]) {
    def xml(implicit graphMLDatatype: Datatype[GraphMLGraphType[V, N, M]]): NodeSeq = {
      graphMLDatatype.serialize(graph)
    }
  }

  object ToGraphML {
    def apply[V, N, M](graph: Graph[GraphMLEdge[V], GraphMLNode[N], GraphMLGraph[M]])(implicit graphMLDatatype: Datatype[GraphMLGraphType[V, N, M]]): NodeSeq =
      graphMLDatatype.serialize(graph)
  }

  object FromGraphML {
    def apply[V, N, M](graphml: String)(implicit graphMLDatatype: Datatype[GraphMLGraphType[V, N, M]]): Either[NonEmptyList[Throwable], GraphMLGraphType[V, N, M]] =
      XMLS.parse(graphml) match {
        case Right(rootElem) if rootElem.label.toLowerCase == "graphml" => graphMLDatatype.deserialize(Seq(rootElem), parseKeys(rootElem)).toEither
        case Right(nonGraphMl) => Left(NonEmptyList.of(new IllegalArgumentException(s"parsed elem is not a graphml element: ${nonGraphMl.toString}")))
        case Left(error) => Left(NonEmptyList.of(error))
      }
  }

  implicit class GraphMLConverterOps[V, N, M](graph: Graph[V, N, M])(implicit nodeIdentity: Identifiable[N]) {
    def asGraphML(withEdgeLabels: Boolean = true): GraphMLGraphType[V, N, M] = {
      val nodeContext: collection.Map[GraphMLNode[N], NodeContext[GraphMLEdge[V], GraphMLNode[N]]] = graph.nodeContext.map {
        case (node, context) =>
          val nodeId = nodeIdentity.id(node)
          (GraphMLNode(nodeId, node, Some(nodeId)), context.map(
            contextNode => {
              val contextNodeId = nodeIdentity.id(contextNode)
              GraphMLNode(contextNodeId, contextNode, Some(contextNodeId))
            },
            contextEdge => {
              val edgeId = s"${contextEdge.head.toString}-${contextEdge.tail.toString}"
              val sourceId = nodeIdentity.id(contextEdge.head)
              val targetId = nodeIdentity.id(contextEdge.tail)
              GraphMLEdge(edgeId, contextEdge.value, Some(sourceId), Some(targetId), if (withEdgeLabels) Some(contextEdge.value.toString) else None)
            }))
      }

      ImmutableGraph(GraphMLGraph(graph.value, Some("G"), Seq.empty), nodeContext)
    }
  }

}
