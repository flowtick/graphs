package com.flowtick.graphs

package object defaults {
  implicit def identifiableEdgeString[E, N]: Identifiable[Edge[E, N], String] =
    Identifiable.identify[Edge[E, N], String] { edge =>
      s"${edge.from}-${edge.to}"
    }

  implicit val identifiableString: Identifiable[String, String] = Identifiable.identity
  implicit val identifiableUnit: Identifiable[Unit, String] = Identifiable.identify(_ => "()")

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

  object label {
    implicit val unitLabel: Labeled[Unit, String] = _ => "()"
    implicit val intOptLabel: Labeled[Int, Option[String]] = number => Some(number.toString)
    implicit val intLabel: Labeled[Int, String] = number => number.toString

    implicit def labeledEdgeString[E, N](implicit labeled: Labeled[E, String]): Labeled[Edge[E, N], String] =
      Labeled.label[Edge[E, N], String](edge => labeled(edge.value))

    implicit val stringOptLabel: Labeled[String, Option[String]] = string => Some(string)
    implicit def edgeStringOptLabel[E, N](implicit edgeLabel: Labeled[Edge[E, N], String]): Labeled[Edge[E, N], Option[String]] =
      edge => Some(edgeLabel(edge))
  }

  implicit class DefaultEdgeBuilder[N, I](from: N) {
    def -->[E](value: E, to: N): Edge[E, N] = Edge(value, from, to)
    def -->(to: N): Edge[Unit, N] = Edge((), from, to)
  }

  implicit val stringLabel: Labeled[String, String] = (string: String) => string

  implicit def numericEdgeLabel[E, N](implicit numeric: Numeric[E]): Labeled[Edge[E, N], E] = new Labeled[Edge[E, N], E] {
    override def apply(edge: Edge[E, N]): E = edge.value
  }
}
