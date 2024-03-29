package com.flowtick.graphs

package object defaults {
  implicit val identifiableString: Identifiable[String] =
    Identifiable.identify(identity)
  implicit val identifiableUnit: Identifiable[Unit] =
    Identifiable.identify(_ => "()")
  implicit val identifiableInt: Identifiable[Int] =
    Identifiable.identify(int => int.toString)

  private final case class IdentifiableOption[T](id: Identifiable[T])
      extends Identifiable[Option[T]] {
    override def apply(value: Option[T]): String = value.map(id(_)).getOrElse("none")
  }

  implicit def identifiableOption[T](implicit id: Identifiable[T]): Identifiable[Option[T]] =
    IdentifiableOption(id)

  object id {
    implicit val identifyAny: Identifiable[Any] = (value: Any) => value.toString
  }

  object label {
    implicit val unitLabel: Labeled[Unit, String] = _ => "()"
    implicit val intOptLabel: Labeled[Int, Option[String]] = number => Some(number.toString)
    implicit val intLabel: Labeled[Int, String] = number => number.toString

    implicit def labeledEdgeString[E, N](implicit
        labeled: Labeled[E, String]
    ): Labeled[Edge[E], String] =
      Labeled.label[Edge[E], String](edge => labeled(edge.value))

    implicit def labeledNodeString[N](implicit
        labeled: Labeled[N, String]
    ): Labeled[Node[N], String] =
      Labeled.label[Node[N], String](node => labeled(node.value))

    implicit val stringOptLabel: Labeled[String, Option[String]] = string => Some(string)

    implicit def edgeStringOptLabel[E, N](implicit
        edgeLabel: Labeled[Edge[E], String]
    ): Labeled[Edge[E], Option[String]] =
      edge => Some(edgeLabel(edge))

    implicit def nodeStringOptLabel[N](implicit
        nodeLabel: Labeled[Node[N], String]
    ): Labeled[Node[N], Option[String]] =
      node => Some(nodeLabel(node))
  }

  implicit class DefaultRelationBuilder[N](from: N) {

    /** create a relation
      *
      * @param value
      *   value of the relation
      * @param to
      *   target / co-domain / image of `from`
      * @param nodeId
      *   identity to use
      * @return
      *   new directed relation between `to` and `from`
      */
    def -->[E](value: E, to: N)(implicit
        nodeId: Identifiable[N]
    ): Relation[E, N] =
      Relation(value, Node(nodeId(from), from), Node(nodeId(to), to))
    def -->(to: N)(implicit nodeId: Identifiable[N]): Relation[Unit, N] =
      Relation((), Node(nodeId(from), from), Node(nodeId(to), to))

    /** create a symmetric relation
      *
      * @param value
      *   value of the relation
      * @param to
      *   related node
      * @param nodeId
      *   identity to use
      * @return
      *   new symmetric relation between `to` and `from`
      */
    def ~~~[E](value: E, to: N)(implicit
        nodeId: Identifiable[N]
    ): Relation[E, N] = Relation(
      value,
      Node(nodeId(from), from),
      Node(nodeId(to), to),
      symmetric = true
    )
    def ~~~(to: N)(implicit nodeId: Identifiable[N]): Relation[Unit, N] =
      Relation(
        (),
        Node(nodeId(from), from),
        Node(nodeId(to), to),
        symmetric = true
      )
  }

  implicit val stringLabel: Labeled[String, String] = (string: String) => string

  implicit def numericEdgeLabel[E, N](implicit
      numeric: Numeric[E]
  ): Labeled[Edge[E], E] = new Labeled[Edge[E], E] {
    override def apply(edge: Edge[E]): E = edge.value
  }
}
