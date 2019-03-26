import com.flowtick.graphs.defaults._
import com.flowtick.graphs.defaults.directed._
import com.flowtick.graphs.algorithm._

trait TopologicalSortingExample {
  lazy val graph = directedGraph.from(Set(
    n("A") --> n("B"),
    n("B") --> n("C"),
    n("D") --> n("A")))

  lazy val clothingDependencies = directedGraph.from(Set(
    n("Underpants") --> n("Pants"),
    n("Pants") --> n("Coat"),
    n("Pullover") --> n("Coat"),
    n("Undershirt") --> n("Pullover"),
    n("Pants") --> n("Shoes"),
    n("Socks") --> n("Shoes")))

  println(graph.topologicalSort)
  // List(D, A, B, C)

  println(clothingDependencies.topologicalSort)
  // List(Undershirt, Pullover, Underpants, Pants, Coat, Socks, Shoes)
}
