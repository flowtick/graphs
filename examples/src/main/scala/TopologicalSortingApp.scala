import com.flowtick.graphs.defaults._
import com.flowtick.graphs.defaults.directed._
import com.flowtick.graphs.algorithm._

object TopologicalSortingApp extends App {
  lazy val graph = defaultGraph.from(Set(
    n("A") --> n("B"),
    n("B") --> n("C"),
    n("D") --> n("A")))

  lazy val clothingDependencies = defaultGraph.from(Set(
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
