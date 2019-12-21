import com.flowtick.graphs.defaults._
import com.flowtick.graphs.cat.instances._
import cats.implicits._
import cats.kernel.Monoid
import com.flowtick.graphs.Graph

trait CatsExample {
  implicit val monoid = Monoid[Graph[Unit, String, Unit]]

  val someGraph: Graph[Unit, String, Unit] = Graph.fromEdges(Set(
    n("1") --> n("2"),

    n("2") --> n("3"),
    n("2") --> n("4")))

  val anotherGraph: Graph[Unit, String, Unit] = Graph.fromEdges(Set(
    n("2") --> n("3"),

    n("4") --> n("3"),
    n("4") --> n("5")))

  val combined: Graph[Unit, String, Unit] = monoid.combine(someGraph, anotherGraph)

  println(combined.edges)
  // Set(4 --> 3[()], 4 --> 5[()], 1 --> 2[()], 2 --> 3[()], 2 --> 4[()])
}
