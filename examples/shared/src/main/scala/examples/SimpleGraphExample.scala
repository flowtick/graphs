package examples

trait SimpleGraphExample {
  // #simple_graph
  import com.flowtick.graphs._
  import com.flowtick.graphs.defaults._

  val graph: Graph[Unit, String] =
    Graph.fromEdges(Set("A" --> "B", "B" --> "C", "D" --> "A"))

  println(graph.edges)
  // #simple_graph
}
