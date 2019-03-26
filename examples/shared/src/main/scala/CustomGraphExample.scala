import com.flowtick.graphs.defaults._
import com.flowtick.graphs.defaults.directed._

trait CustomGraphExample {
  // #custom_graph
  case class MyNode(id: String, someCustomProperty: String)

  val graph: DefaultGraph[Unit, MyNode, Unit] = directedGraph.from(Set(
    n(MyNode("first_node", "My first node")) --> n(MyNode("second_node", "My second node"))))

  println(directedGraph.edges(graph))
  // Set(MyNode(first_node,My first node) --> MyNode(second_node,My second node)[()])
  // #custom_graph
}
