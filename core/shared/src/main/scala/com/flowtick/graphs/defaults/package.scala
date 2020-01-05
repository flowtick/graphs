package com.flowtick.graphs

package object defaults {
  def n[X](value: X, label: Option[String] = None) = Node[X](value, label)

  object id {
    def edge[V, N] = (edge: Edge[V, N]) =>
      s"${edge.head.toString}-${edge.tail.toString}"

    implicit val identifiableEdgeString = Identifiable.identify[Edge[_, String]](edge(_))

    implicit val identifiableEdgeInt = Identifiable.identify[Edge[_, Int]](edge(_))
  }

  implicit val identifiableUnit: Identifiable[Unit] = new Identifiable[Unit] {
    override def id(string: Unit): String = "()"
  }

  implicit val identifiableString: Identifiable[String] = new Identifiable[String] {
    override def id(string: String): String = string
  }

  implicit def identifiableNumeric[N](implicit numeric: Numeric[N]): Identifiable[N] = new Identifiable[N] {
    override def id(number: N): String = number match {
      case _: Integer => number.toString
      case _: Long => number.toString
      case _ => numeric.toDouble(number).toString
    }
  }

  implicit val unitLabel: Labeled[Unit, String] = new Labeled[Unit, String] {
    override def label(edge: Unit): Option[String] = None
  }

  implicit val stringLabel: Labeled[String, String] = new Labeled[String, String] {
    override def label(string: String): Option[String] = Some(string)
  }

  implicit def numericLabel[T](implicit numeric: Numeric[T]): Labeled[T, String] = new Labeled[T, String] {
    override def label(number: T): Option[String] = Some(numeric.toDouble(number).toString)
  }

  implicit def labeledEdge[V, N](implicit labeled: Labeled[V, String]): Labeled[Edge[V, N], String] = new Labeled[Edge[V, N], String] {
    override def label(e: Edge[V, N]): Option[String] = labeled.label(e.value)
  }

  implicit class DefaultEdgeBuilder[X](node: Node[X]) {
    def -->[V](value: V, to: Node[X]): Edge[V, X] = Edge[V, X](value, node.value, to.value)
    def -->(to: Node[X]): Edge[Unit, X] = Edge[Unit, X]((), node.value, to.value)
  }
}
