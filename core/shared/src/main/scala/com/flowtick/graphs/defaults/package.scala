package com.flowtick.graphs

package object defaults {
  def n[X](value: X, label: Option[String] = None) = Node[X](value, label)

  implicit def identifiableString[X]: Identifiable[String] = new Identifiable[String] {
    override def id(string: String): String = string
  }

  implicit def identifiableNumeric[N](implicit numeric: Numeric[N]): Identifiable[N] = new Identifiable[N] {
    override def id(number: N): String = numeric.toDouble(number).toString
  }

  implicit def unitLabel: Labeled[Unit, String] = new Labeled[Unit, String] {
    override def label(edge: Unit): Option[String] = None
  }

  implicit def stringLabel: Labeled[String, String] = new Labeled[String, String] {
    override def label(string: String): Option[String] = Some(string)
  }

  implicit def numericLabel[T](implicit numeric: Numeric[T]): Labeled[T, String] = new Labeled[T, String] {
    override def label(number: T): Option[String] = Some(numeric.toDouble(number).toString)
  }

  implicit def labeledEdge[V, N](implicit labeled: Labeled[V, String]): Labeled[Edge[V, N], String] = new Labeled[Edge[V, N], String] {
    override def label(e: Edge[V, N]): Option[String] = labeled.label(e.value)
  }

  implicit class EdgeBuilder[X](node: Node[X]) {
    def -->[V](value: V, to: Node[X]): Edge[V, X] = Edge[V, X](value, node.value, to.value)
    def -->[V](to: Node[X]): Edge[Unit, X] = Edge[Unit, X]((), node.value, to.value)
  }
}
