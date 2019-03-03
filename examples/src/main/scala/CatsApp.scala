import com.flowtick.graphs.defaults._
import com.flowtick.graphs.defaults.directed._
import com.flowtick.graphs.cat._
import cats.implicits._
import cats.kernel.Monoid

object CatsApp extends App {
  implicit val monoid: Monoid[DefaultGraph[Edge[Unit, String], String, Unit]] = graphMonoid

  val someGraph = defaultGraph.from(Set(
    n("1") --> n("2"),

    n("2") --> n("3"),
    n("2") --> n("4")))

  val anotherGraph = defaultGraph.from(Set(
    n("2") --> n("3"),

    n("4") --> n("3"),
    n("4") --> n("5")))

  val combined = monoid.combine(someGraph, anotherGraph)

  println(defaultGraph.edges(combined))
  // Set(4 --> 3[()], 4 --> 5[()], 1 --> 2[()], 2 --> 3[()], 2 --> 4[()])
}
