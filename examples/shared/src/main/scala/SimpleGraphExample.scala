import com.flowtick.graphs._
import com.flowtick.graphs.defaults._

trait SimpleGraphExample {
  val graph: Graph[Unit, String, Unit] = Graph.fromEdges(Set(
    n("A") --> n("B"),
    n("B") --> n("C"),
    n("D") --> n("A")))

  println(graph.edges)
}
