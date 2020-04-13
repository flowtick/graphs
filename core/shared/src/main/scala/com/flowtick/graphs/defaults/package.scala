package com.flowtick.graphs

package object defaults {
  implicit def identifiableEdgeString[E, N]: Identifiable[Edge[E, N], String] =
    Identifiable.identify[Edge[E, N], String] { edge =>
      s"${edge.from}-${edge.to}"
    }

  implicit val identifiableString: Identifiable[String, String] = new Identifiable[String, String] {
    override def apply(string: String): String = string
  }

  object id {
    implicit def int[N]: Identifiable[N, Int] = new Identifiable[N, Int] {
      override def apply(number: N): Int = number match {
        case aInt: Integer => aInt
        case aLong: Long => aLong.toInt
        case other => other.hashCode()
      }
    }

    implicit def long[N]: Identifiable[N, Long] = new Identifiable[N, Long] {
      override def apply(number: N): Long = number match {
        case aInt: Integer => aInt.toLong
        case aLong: Long => aLong
        case other => other.hashCode()
      }
    }

    implicit def string[N]: Identifiable[N, String] = new Identifiable[N, String] {
      override def apply(value: N): String = value.hashCode().toString()
    }
  }

  implicit class DefaultEdgeBuilder[N, I](from: N) {
    def -->[E](value: E, to: N): Edge[E, N] = Edge(value, from, to)
    def -->(to: N): Edge[Unit, N] = Edge((), from, to)
  }

  implicit val unitLabel: Labeled[Unit, String] = new Labeled[Unit, String] {
    override def apply(edge: Unit): String = "()"
  }

  implicit val stringLabel: Labeled[String, String] = new Labeled[String, String] {
    override def apply(string: String): String = string
  }

  implicit def numericEdgeLabel[E, N](implicit numeric: Numeric[E]): Labeled[Edge[E, N], E] = new Labeled[Edge[E, N], E] {
    override def apply(edge: Edge[E, N]): E = edge.value
  }
}
