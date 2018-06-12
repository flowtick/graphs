import com.flowtick.graphs.defaults._
import com.flowtick.graphs.defaults.directed._

import com.flowtick.graphs.cat._
import cats.syntax.monoid._

object CatsApp extends App {
  val someGraph = DefaultGraph.create(Seq(
    n("1") -> n("2"),

    n("2") -> n("3"),
    n("2") -> n("4")))

  val anotherGraph = DefaultGraph.create(Seq(
    n("2") -> n("3"),

    n("4") -> n("3"),
    n("4") -> n("5")))

  val combined = someGraph |+| anotherGraph
  println(combined.edges)
  // Set(DefaultEdge(DefaultNode(4),Some(DefaultNode(3)),true), DefaultEdge(DefaultNode(2),Some(DefaultNode(4)),true), ...
}
