import com.flowtick.graphs.Graph
import com.flowtick.graphs.defaults._

trait CustomGraphExample {
  // #custom_graph
  final case class MyNode(id: String, someCustomProperty: String)

  val graph = Graph.fromEdges(Set(
    MyNode("first_node", "My first node") --> MyNode("second_node", "My second node")))

  println(graph.edges)
  // #custom_graph
}
