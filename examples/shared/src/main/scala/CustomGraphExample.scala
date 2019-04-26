import com.flowtick.graphs.Graph
import com.flowtick.graphs.defaults._

trait CustomGraphExample {
  // #custom_graph
  case class MyNode(id: String, someCustomProperty: String)

  val graph = Graph.from(Set(
    n(MyNode("first_node", "My first node")) --> n(MyNode("second_node", "My second node"))))

  println(graph.edges)
  // Set(MyNode(first_node,My first node) --> MyNode(second_node,My second node)[()])
  // #custom_graph
}