package examples

import cats.kernel.Monoid
import com.flowtick.graphs.Graph
import com.flowtick.graphs.cat.instances._
import com.flowtick.graphs.defaults._

trait CatsExample {
  implicit val monoid = Monoid[Graph[Unit, String]]

  val someGraph: Graph[Unit, String] =
    Graph.fromEdges(Set("1" --> "2", "2" --> "3", "2" --> "4"))

  val anotherGraph: Graph[Unit, String] =
    Graph.fromEdges(Set("2" --> "3", "4" --> "3", "4" --> "5"))

  val combined: Graph[Unit, String] = monoid.combine(someGraph, anotherGraph)

  println(combined.edges)
  // Set(4 --> 3[()], 4 --> 5[()], 1 --> 2[()], 2 --> 3[()], 2 --> 4[()])
}
