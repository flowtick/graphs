import com.flowtick.graphs._
import com.flowtick.graphs.defaults._
import com.flowtick.graphs.algorithm._

trait TopologicalSortingExample {
  lazy val graph = Graph.fromEdges(Set(
    "A" --> "B",
    "B" --> "C",
    "D" --> "A"))

  lazy val clothingDependencies = Graph.fromEdges(Set(
    "Underpants" --> "Pants",
    "Pants" --> "Coat",
    "Pullover" --> "Coat",
    "Undershirt" --> "Pullover",
    "Pants" --> "Shoes",
    "Socks" --> "Shoes"))

  println(graph.topologicalSort)
  // List(D, A, B, C)

  println(clothingDependencies.topologicalSort)
  // List(Undershirt, Pullover, Underpants, Pants, Coat, Socks, Shoes)
}
