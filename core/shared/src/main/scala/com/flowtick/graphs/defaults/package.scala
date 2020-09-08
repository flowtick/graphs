package com.flowtick.graphs

package object defaults {
  implicit val identifiableString: Identifiable[String] = Identifiable.identify(identity)
  implicit val identifiableUnit: Identifiable[Unit] = Identifiable.identify(_ => "()")
  implicit val identifiableInt: Identifiable[Int] = Identifiable.identify(int => int.toString)

  object anyId {
    implicit val identifyAny = new Identifiable[Any] {
      override def apply(value: Any): String = value.toString
    }
  }

  object label {
    implicit val unitLabel: Labeled[Unit, String] = _ => "()"
    implicit val intOptLabel: Labeled[Int, Option[String]] = number => Some(number.toString)
    implicit val intLabel: Labeled[Int, String] = number => number.toString

    implicit def labeledEdgeString[E, N](implicit labeled: Labeled[E, String]): Labeled[Edge[E], String] =
      Labeled.label[Edge[E], String](edge => labeled(edge.value))

    implicit def labeledNodeString[N](implicit labeled: Labeled[N, String]): Labeled[Node[N], String] =
      Labeled.label[Node[N], String](node => labeled(node.value))

    implicit val stringOptLabel: Labeled[String, Option[String]] = string => Some(string)

    implicit def edgeStringOptLabel[E, N](implicit edgeLabel: Labeled[Edge[E], String]): Labeled[Edge[E], Option[String]] =
      edge => Some(edgeLabel(edge))

    implicit def nodeStringOptLabel[N](implicit nodeLabel: Labeled[Node[N], String]): Labeled[Node[N], Option[String]] =
      node => Some(nodeLabel(node))
  }

  implicit class DefaultEdgeBuilder[N](from: N) {
    def -->[E](value: E, to: N)(implicit nodeId: Identifiable[N]): Relation[E, N] = Relation(value, Node(nodeId(from), from), Node(nodeId(to), to))
    def -->(to: N)(implicit nodeId: Identifiable[N]): Relation[Unit, N] = Relation((), Node(nodeId(from), from), Node(nodeId(to), to))
  }

  implicit val stringLabel: Labeled[String, String] = (string: String) => string

  implicit def numericEdgeLabel[E, N](implicit numeric: Numeric[E]): Labeled[Edge[E], E] = new Labeled[Edge[E], E] {
    override def apply(edge: Edge[E]): E = edge.value
  }
}
