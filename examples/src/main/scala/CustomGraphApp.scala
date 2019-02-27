import com.flowtick.graphs._
import com.flowtick.graphs.defaults._
import com.flowtick.graphs.defaults.directed._

object CustomGraphApp extends App {
  // #custom_graph
  case class MyNode(id: String, someCustomProperty: String)

  implicit def identifiableNode = new Identifiable[MyNode] {
    override def id(node: MyNode): String = node.id
  }

  val graph = defaultGraph.from(Set(
    n(MyNode("first_node", "My first node")) --> n(MyNode("second_node", "My second node"))))

  println(defaultGraph.edges(graph))
  // Set(MyNode(first_node,My first node) --> MyNode(second_node,My second node)[()])
  // #custom_graph
}
