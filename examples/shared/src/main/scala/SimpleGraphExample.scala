import com.flowtick.graphs._
import com.flowtick.graphs.defaults._

trait SimpleGraphExample {
  val graph: Graph[Unit, String, Unit] = Graph.fromEdges(Set(
    "A" --> "B",
    "B" --> "C",
    "D" --> "A"))

  println(graph.edges)
}
