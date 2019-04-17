import com.flowtick.graphs.defaults._
import com.flowtick.graphs.defaults.directed._

trait SimpleGraphExample {
  val graph = Graph.from(Set(
    n("A") --> n("B"),
    n("B") --> n("C"),
    n("D") --> n("A")))

  println(defaultGraph.edges(graph))
  // Set(A --> B[()], B --> C[()], D --> A[()])
}
