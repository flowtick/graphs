import com.flowtick.graphs.defaults._
import com.flowtick.graphs.cat.instances._
import cats.implicits._
import cats.kernel.Monoid
import com.flowtick.graphs.Graph

trait CatsExample {
  implicit val monoid = Monoid[Graph[Unit, Unit, String]]

  val someGraph: Graph[Unit, Unit, String] = Graph.fromEdges(Set(
    "1" --> "2",

    "2" --> "3",
    "2" --> "4"))

  val anotherGraph: Graph[Unit, Unit, String] = Graph.fromEdges(Set(
    "2" --> "3",

    "4" --> "3",
    "4" --> "5"))

  val combined: Graph[Unit, Unit, String] = monoid.combine(someGraph, anotherGraph)

  println(combined.edges)
  // Set(4 --> 3[()], 4 --> 5[()], 1 --> 2[()], 2 --> 3[()], 2 --> 4[()])
}
