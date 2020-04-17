import com.flowtick.graphs._
import com.flowtick.graphs.defaults._

trait SimpleGraphExample {
  val graph: Graph[Unit, Unit, String] = Graph.fromEdges(Set(
    "A" --> "B",
    "B" --> "C",
    "D" --> "A"))

  println(graph.edges)
}
