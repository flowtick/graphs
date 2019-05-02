package com.flowtick.graphs

import cats.data.Validated._
import cats.data._
import cats.implicits._
import com.flowtick.graphs.graphml.GraphMLDatatype.parseKeys
import shapeless._
import shapeless.ops.record.Keys
import shapeless.ops.traversable.FromTraversable
import xmls.XMLS

import scala.collection.GenTraversable
import scala.reflect.ClassTag
import scala.util.{ Either, Left, Right }
import scala.xml.NodeSeq

package object graphml {

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
  class FromList[T, Repr <: HList](implicit generic: Generic.Aux[T, Repr], typeable: Typeable[Repr], fromTraversable: FromTraversable[Repr]) {
    import shapeless.syntax.std.traversable._

    def apply(l: GenTraversable[_]): Option[T] = {
      l.toHList[Repr].map(generic.from)
    }
  }

  trait Serializer[T] {
    def serialize(value: T): NodeSeq
    def keys: Seq[GraphMLKey] = Seq.empty
  }

  trait Deserializer[T] {
    def deserialize(from: NodeSeq, graphKeys: scala.collection.Map[String, GraphMLKey]): ValidatedNel[Throwable, T]
  }

  trait Datatype[T] extends Serializer[T] with Deserializer[T]

  object Datatype extends LabelledProductTypeClassCompanion[Datatype] {
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

  implicit object DatatypeString extends Datatype[String] {
    def serialize(value: String): NodeSeq = <value>{ value }</value>

    def deserialize(from: NodeSeq, graphKeys: scala.collection.Map[String, GraphMLKey]): Validated[NonEmptyList[Throwable], String] = from match {
      case <value>{ value }</value> => value.text.validNel
      case _ => new RuntimeException("Bad string XML").invalidNel
    }
  }

  implicit object DatatypeUnit extends Datatype[Unit] {
    def serialize(value: Unit): NodeSeq = <!-- unit -->
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

  implicit def genericFromList[T, Repr <: HList](implicit generic: Generic.Aux[T, Repr], typeable: Typeable[Repr], fromTraversable: FromTraversable[Repr]): FromList[T, Repr] = new FromList[T, Repr]

  implicit def graphMLUnitNodeDataType: GraphMLNodeDatatype[Unit, HNil, HNil] = new GraphMLNodeDatatype[Unit, HNil, HNil]

  implicit def graphMLNodeDataType[T, Repr <: HList, FromRepr <: HList](implicit classTag: ClassTag[T], genericValue: shapeless.LabelledGeneric.Aux[T, Repr], keys: Keys[Repr], fromList: FromList[T, FromRepr]): GraphMLNodeDatatype[T, Repr, FromRepr] = new GraphMLNodeDatatype[T, Repr, FromRepr]

  implicit def graphMLDataType[V, N, M](implicit
    identifiable: Identifiable[GraphMLNode[N]],
    edgeLabel: Labeled[Edge[GraphMLEdge[Unit], GraphMLNode[N]], String],
    nodeDataType: Datatype[GraphMLNode[N]]): Datatype[Graph[GraphMLEdge[Unit], GraphMLNode[N], GraphMLGraph[Unit]]] = new GraphMLDatatype[V, N, M]

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

  implicit class GraphMLOps[V, N, M](graph: Graph[GraphMLEdge[V], GraphMLNode[N], GraphMLGraph[M]]) {
    def xml(implicit graphMLDatatype: Datatype[Graph[GraphMLEdge[V], GraphMLNode[N], GraphMLGraph[M]]]): NodeSeq = {
      graphMLDatatype.serialize(graph)
    }
  }

  implicit class GraphMLConverterOps[V, N, M](graph: Graph[V, N, M]) {
    def toGraphML: Graph[GraphMLEdge[V], GraphMLNode[N], GraphMLGraph[M]] = Graph(
      GraphMLGraph(graph.value, Some("G"), Seq.empty), graph.edges.map(edge => {
        val id = s"${edge.head}-${edge.tail}"
        Edge(GraphMLEdge(id, edge.value), GraphMLNode(edge.head.toString, edge.head), GraphMLNode(edge.head.toString, edge.tail))
      }), graph.nodeContext.map {
        case (node, context) => (GraphMLNode(node.toString, node), context.map(
          node => GraphMLNode(node.toString, node),
          value => GraphMLEdge(value.toString, value)))
      })
  }

  def fromGraphML[V, N, M](graphml: String)(implicit graphMLDatatype: Datatype[Graph[GraphMLEdge[V], GraphMLNode[N], GraphMLGraph[M]]]): Either[NonEmptyList[Throwable], Graph[GraphMLEdge[V], GraphMLNode[N], GraphMLGraph[M]]] = {
    XMLS.parse(graphml) match {
      case Right(rootElem) if rootElem.label.toLowerCase == "graphml" => graphMLDatatype.deserialize(Seq(rootElem), parseKeys(rootElem)).toEither
      case Right(nonGraphMl) => Left(NonEmptyList.of(new IllegalArgumentException(s"parsed elem is not a graphml element: $nonGraphMl")))
      case Left(error) => Left(NonEmptyList.of(error))
    }
  }

}
