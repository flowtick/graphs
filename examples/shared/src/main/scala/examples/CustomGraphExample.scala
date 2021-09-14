package examples

import com.flowtick.graphs.defaults._
import com.flowtick.graphs.{Graph, Identifiable}

trait CustomGraphExample {
  // #custom_graph
  final case class MyNode(id: String, someCustomProperty: String)

  implicit val myNodeId: Identifiable[MyNode] = new Identifiable[MyNode] {
    override def apply(value: MyNode): String = value.id
  }

  val graph = Graph.fromEdges(
    Set(
      MyNode("first_node", "My first node") --> MyNode(
        "second_node",
        "My second node"
      )
    )
  )

  println(graph.edges)
  // #custom_graph
}
