package com.flowtick.graphs

import cats.data.Validated._
import cats.data._
import cats.implicits._
import com.flowtick.graphs.graphml.GraphMLDatatype.parseKeys
import com.flowtick.graphs.style._
import xmls.XMLS

import scala.collection.GenTraversable
import scala.reflect.ClassTag
import scala.util.{Either, Left, Right}
import scala.xml.{Elem, NodeSeq, Text}

package object graphml {
  trait Serializer[T] {
    def serialize(value: T, targetHint: Option[String]): NodeSeq
    def keys(targetHint: Option[String]): Seq[GraphMLKey]
  }

  trait Deserializer[T] {
    def deserialize(
        from: NodeSeq,
        graphKeys: scala.collection.Map[String, GraphMLKey],
        targetHint: Option[String]
    ): ValidatedNel[Throwable, T]
  }

  trait FromList[T, R] {
    def apply(l: GenTraversable[_]): Option[T]
  }

  trait Datatype[T] extends Serializer[T] with Deserializer[T]

  object generic {
    import shapeless._
    import shapeless.ops.record.Keys
    import shapeless.ops.traversable.FromTraversable

    /** there is no default Typeable for LabelledGeneric (more precisely the KeyTags), so we need to
      * fallback to the generic representation of T to create the value from a list.
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
        fromTraversable: FromTraversable[Repr]
    ) extends FromList[T, Repr] {
      import shapeless.syntax.std.traversable._

      def apply(l: GenTraversable[_]): Option[T] = {
        l.toHList[Repr].map(generic.from)
      }
    }

    object Datatype extends LabelledProductTypeClassCompanion[Datatype] {
      def apply[T](implicit dt: Datatype[T]): Datatype[T] = dt

      object typeClass extends LabelledProductTypeClass[Datatype] {
        def emptyProduct: Datatype[HNil] = new Datatype[HNil] {
          def serialize(value: HNil, targetHint: Option[String]): NodeSeq = Nil
          def deserialize(
              from: NodeSeq,
              graphKeys: scala.collection.Map[String, GraphMLKey],
              targetHint: Option[String]
          ) = Valid(HNil)

          override def keys(targetHint: Option[String]): Seq[GraphMLKey] =
            Seq.empty
        }

        def product[H, T <: HList](
            name: String,
            dh: Datatype[H],
            dt: Datatype[T]
        ): Datatype[H :: T] = new Datatype[H :: T] {
          def serialize(value: H :: T, targetHint: Option[String]): NodeSeq =
            dh.serialize(value.head, targetHint) ++ dt.serialize(
              value.tail,
              targetHint
            )

          def deserialize(
              from: NodeSeq,
              graphKeys: scala.collection.Map[String, GraphMLKey],
              targetHint: Option[String]
          ): ValidatedNel[Throwable, H :: T] =
            (
              from.headOption
                .map(dh.deserialize(_, graphKeys, targetHint))
                .getOrElse(
                  invalidNel[Throwable, H](
                    new IllegalArgumentException(
                      "can't serialize empty node sequence"
                    )
                  )
                ),
              dt.deserialize(from.drop(1), graphKeys, targetHint)
            ).mapN(_ :: _)

          override def keys(targetHint: Option[String]): Seq[GraphMLKey] =
            dh.keys(targetHint) ++ dt.keys(targetHint)
        }

        def project[F, G](
            instance: => Datatype[G],
            to: F => G,
            from: G => F
        ): Datatype[F] = new Datatype[F] {
          def serialize(value: F, targetHint: Option[String]): NodeSeq =
            instance.serialize(to(value), targetHint)
          def deserialize(
              nodes: NodeSeq,
              graphKeys: scala.collection.Map[String, GraphMLKey],
              targetHint: Option[String]
          ): Validated[NonEmptyList[Throwable], F] =
            instance.deserialize(nodes, graphKeys, targetHint).map(from)

          override def keys(targetHint: Option[String]): Seq[GraphMLKey] =
            instance.keys(targetHint)
        }
      }
    }

    implicit def genericFromList[T, Repr <: HList](implicit
        generic: Generic.Aux[T, Repr],
        typeable: Typeable[Repr],
        fromTraversable: FromTraversable[Repr]
    ): FromListGeneric[T, Repr] = new FromListGeneric[T, Repr]
    implicit def stringFromList(implicit
        generic: Generic.Aux[String, String :: HNil],
        typeable: Typeable[String :: HNil],
        fromTraversable: FromTraversable[String :: HNil]
    ): FromListGeneric[String, String :: HNil] =
      new FromListGeneric[String, String :: HNil]
    implicit def graphMLDatatypeGeneric[T, Repr <: HList, FromRepr <: HList](implicit
        classTag: ClassTag[T],
        genericValue: shapeless.LabelledGeneric.Aux[T, Repr],
        keys: Keys[Repr],
        fromList: FromListGeneric[T, FromRepr]
    ) = new GenericDataType[T, Repr, FromRepr]

    /** * see FromList to understand why we need two representations * * the good news is that an
      * implicit LabelledGeneric will also be provide an implicit Generic
      *
      * @param genericValue
      * @param fromList
      * @param genericValueKeys
      * @param classTag
      * @tparam T
      * @tparam Repr
      * @tparam FromRepr
      * @return
      */
    class GenericDataType[T, Repr <: HList, FromRepr <: HList](implicit
        genericValue: LabelledGeneric.Aux[T, Repr],
        fromList: FromList[T, FromRepr],
        genericValueKeys: Keys[Repr],
        classTag: ClassTag[T]
    ) extends Datatype[T] {
      lazy val valueKeys: List[String] =
        genericValueKeys().runtimeList.map(_.asInstanceOf[Symbol].name)

      override def keys(targetHint: Option[String]): Seq[GraphMLKey] =
        valueKeys.map(key => {
          val prefixValue = targetHint.getOrElse("")
          GraphMLKey(
            id = s"${prefixValue}_$key",
            name = Some(key),
            targetHint = targetHint,
            typeHint = Some("string"),
            graphsType = Some(classTag.toString())
          )
        })

      override def deserialize(
          from: NodeSeq,
          graphKeys: collection.Map[String, GraphMLKey],
          targetHint: Option[String]
      ): ValidatedNel[Throwable, T] = {
        val values = from
          .collect {
            case e: Elem if e.label == "data" && e.attribute("key").isDefined =>
              val valueType = e.attribute("type").getOrElse(List.empty).mkString

              if (valueType == "double") e.text.toDouble
              else if (valueType == "integer") e.text.toInt
              else e.text
          }
          .take(genericValueKeys().runtimeLength)

        fromList(values)
          .map(value => validNel(value))
          .getOrElse(
            invalidNel(
              new IllegalStateException(
                s"unable to parse value from properties: ${from.toString}"
              )
            )
          )
      }

      override def serialize(value: T, targetHint: Option[String]): NodeSeq = {
        val prefix = targetHint.map(_ ++ "_").getOrElse("")

        genericValueKeys().runtimeList
          .zip(genericValue.to(value).runtimeList)
          .map {
            case (key: Symbol, value) if value.isInstanceOf[Int] =>
              <data key={s"$prefix${key.name}"} type="integer" >{
                value.toString
              }</data>

            case (key: Symbol, value) if value.isInstanceOf[Double] || value.isInstanceOf[Float] =>
              <data key={s"$prefix${key.name}"} type="double">{
                value.toString
              }</data>

            case (key: Symbol, value) =>
              <data key={s"$prefix${key.name}"} type="string">{
                value.toString
              }</data>

            case (_, _) => <!-- unknown value -->
          }
      }
    }

    implicit object GenericDataType {
      def apply[T, Repr <: HList, FromRepr <: HList](implicit
          classTag: ClassTag[T],
          genericValue: shapeless.LabelledGeneric.Aux[T, Repr],
          keys: Keys[Repr],
          fromList: FromListGeneric[T, FromRepr]
      ) = new GenericDataType[T, Repr, FromRepr]
    }
  }

  implicit def optionalDataType[T](implicit
      dataType: Datatype[T]
  ): Datatype[Option[T]] = new Datatype[Option[T]] {
    override def keys(targetHint: Option[String]): Seq[GraphMLKey] =
      dataType.keys(targetHint)

    def serialize(value: Option[T], targetHint: Option[String]): NodeSeq =
      value match {
        case None              => <!-- empty optional -->
        case Some(actualValue) => dataType.serialize(actualValue, targetHint)
      }

    def deserialize(
        from: NodeSeq,
        graphKeys: scala.collection.Map[String, GraphMLKey],
        targetHint: Option[String]
    ): Validated[NonEmptyList[Throwable], Option[T]] =
      dataType.deserialize(from, graphKeys, targetHint) match {
        case Valid(value) => Valid(Some(value))
        case Invalid(errors) if errors.forall(_.isInstanceOf[NoSuchElementException]) =>
          Valid(None)
        case Invalid(errors) => Invalid(errors)
      }
  }

  implicit object DatatypeString extends Datatype[String] {
    def keyId(targetHint: Option[String]): String =
      targetHint.map(_ ++ "_value").getOrElse("value")

    override def keys(
        targetHint: Option[String]
    ): Seq[com.flowtick.graphs.graphml.GraphMLKey] = {
      Seq(GraphMLKey(keyId(targetHint), targetHint = targetHint))
    }

    def serialize(value: String, targetHint: Option[String]): NodeSeq =
      <data key={keyId(targetHint)}>{scala.xml.PCData(value)}</data>

    def deserialize(
        from: NodeSeq,
        graphKeys: scala.collection.Map[String, GraphMLKey],
        targetHint: Option[String]
    ): Validated[NonEmptyList[Throwable], String] = from
      .collectFirst { case data =>
        data.text.validNel
      }
      .getOrElse(invalidNel(new NoSuchElementException("no string data found")))
  }

  implicit def doubleDataType(implicit
      stringDataType: Datatype[String]
  ): Datatype[Double] = new Datatype[Double] {
    override def keys(targetHint: Option[String]): Seq[GraphMLKey] =
      stringDataType.keys(targetHint)

    def serialize(value: Double, targetHint: Option[String] = None): NodeSeq =
      stringDataType.serialize(value.toString, targetHint)
    def deserialize(
        from: NodeSeq,
        graphKeys: scala.collection.Map[String, GraphMLKey],
        targetHint: Option[String]
    ): Validated[NonEmptyList[Throwable], Double] = stringDataType
      .deserialize(
        from,
        graphKeys: scala.collection.Map[String, GraphMLKey],
        targetHint
      )
      .map(stringValue => stringValue.toDouble)
  }

  implicit def intDataType(implicit
      stringDataType: Datatype[String]
  ): Datatype[Int] = new Datatype[Int] {
    override def keys(targetHint: Option[String]): Seq[GraphMLKey] =
      stringDataType.keys(targetHint)

    def serialize(value: Int, targetHint: Option[String]): NodeSeq =
      stringDataType.serialize(value.toString, targetHint)
    def deserialize(
        from: NodeSeq,
        graphKeys: scala.collection.Map[String, GraphMLKey],
        targetHint: Option[String]
    ): Validated[NonEmptyList[Throwable], Int] = stringDataType
      .deserialize(
        from,
        graphKeys: scala.collection.Map[String, GraphMLKey],
        targetHint
      )
      .map(stringValue => stringValue.toInt)
  }

  implicit def graphMLDataType[E, N](implicit
      identifiable: Identifiable[GraphMLNode[N]],
      edgeLabel: Labeled[Edge[GraphMLEdge[E]], String],
      nodeDataType: Datatype[N],
      edgeDataType: Datatype[E]
  ): Datatype[GraphMLGraph[E, N]] = GraphMLDatatype[E, N]

  def ml[N](nodeValue: N, id: Option[String] = None): GraphMLNode[N] =
    GraphMLNode(id.getOrElse(nodeValue.toString), nodeValue, None, None, None)

  implicit class GraphMLEdgeBuilder[X](node: GraphMLNode[X]) {
    def -->[V](
        value: V,
        to: GraphMLNode[X]
    ): Relation[GraphMLEdge[V], GraphMLNode[X]] = Relation(
      GraphMLEdge(s"${node.id}-${to.id}", value, Some(node.id), Some(to.id)),
      Node.of(node),
      Node.of(to)
    )
    def -->(to: GraphMLNode[X]): Relation[GraphMLEdge[Unit], GraphMLNode[X]] =
      Relation(
        GraphMLEdge(s"${node.id}-${to.id}", (), Some(node.id), Some(to.id)),
        Node.of(node),
        Node.of(to)
      )
  }

  implicit def graphMLNodeIdentifiable[N]: Identifiable[GraphMLNode[N]] =
    (node: GraphMLNode[N]) => node.id

  implicit def graphMLEdgeLabel[V, N]: Labeled[Edge[GraphMLEdge[V]], String] =
    (edge: Edge[GraphMLEdge[V]]) => edge.value.id

  implicit class GraphMLOps[E, N](graph: GraphMLGraph[E, N]) {
    def xml(implicit graphMLDatatype: Datatype[GraphMLGraph[E, N]]): NodeSeq = {
      graphMLDatatype.serialize(graph, None)
    }
  }

  object ToGraphML {
    def apply[E, N](graph: GraphMLGraph[E, N])(implicit
        graphMLDatatype: Datatype[GraphMLGraph[E, N]]
    ): NodeSeq =
      graphMLDatatype.serialize(graph, None)
  }

  def timed[T](label: String, block: => T): T = {
    val start = System.currentTimeMillis()
    val result = block
    val total = System.currentTimeMillis() - start
    println(s"$label took: $total")
    result
  }

  object FromGraphML {
    def apply[E, N](graphml: String)(implicit
        graphMLDatatype: Datatype[GraphMLGraph[E, N]]
    ): Either[NonEmptyList[Throwable], GraphMLGraph[E, N]] =
      timed("xml parse", XMLS.parse(graphml)) match {
        case Right(rootElem) if rootElem.label.toLowerCase == "graphml" =>
          timed(
            "deser",
            graphMLDatatype.deserialize(
              Seq(rootElem),
              parseKeys(rootElem),
              None
            )
          ).toEither
        case Right(nonGraphMl) =>
          Left(
            NonEmptyList.of(
              new IllegalArgumentException(
                s"parsed elem is not a graphml element: ${nonGraphMl.toString}"
              )
            )
          )
        case Left(error) => Left(NonEmptyList.of(error))
      }
  }

  implicit class GraphMLConverterOps[E, N](graph: Graph[E, N])(implicit
      edgeLabel: Labeled[Edge[E], Option[String]],
      nodeLabel: Labeled[Node[N], Option[String]]
  ) {

    def asGraphML(
        nodeShape: Option[(Option[String], N) => NodeShape] = None
    ): GraphMLGraph[E, N] = {
      val mlNodes: Iterable[Node[GraphMLNode[N]]] = graph.nodes.map { node =>
        node.copy(value =
          GraphMLNode(
            node.id,
            node.value,
            nodeShape.map(f => f(nodeLabel(node), node.value)),
            None,
            nodeLabel(node)
          )
        )
      }

      val mlEdges: Iterable[Edge[GraphMLEdge[E]]] = graph.edges.map { edge =>
        val mlEdge = GraphMLEdge(
          edge.id,
          edge.value,
          Some(edge.from),
          Some(edge.to),
          Some(EdgeShape()),
          None,
          labelValue = edgeLabel(edge)
        )

        Edge.of(mlEdge, edge.from, edge.to)
      }

      GraphMLGraph(Graph(edges = mlEdges, nodes = mlNodes), GraphMLMeta())
    }
  }

}
