import com.flowtick.graphs._

object CustomGraphApp extends App {
  // #custom_graph
  case class MyNode(id: String, someCustomProperty: String)
  case class MyEdge(label: Option[String] = None, source: MyNode, target: MyNode) extends Edge[MyEdge, MyNode] {
    override def value: MyEdge = this
    override def predecessors: Set[MyNode] = Set(source)
    override def successors: Set[MyNode] = Set(target)
  }

  object MyGraph {
    implicit def identifiableNode = new Identifiable[MyNode] {
      override def id(node: MyNode): String = node.id
    }

    implicit def edgeBuilder = new EdgeBuilder[MyNode, MyEdge, ((MyNode, MyNode), Option[String])] {
      override def create(from: ((MyNode, MyNode), Option[String]))(implicit identifiable: Identifiable[MyNode]): MyEdge = {
        val ((source, target), label) = from
        MyEdge(label, source, target)
      }
    }

    def create = Graph.create[MyNode, MyEdge, ((MyNode, MyNode), Option[String])] _
  }

  val graph = MyGraph.create(
    Seq((MyNode("first_node", "My first node") -> MyNode("second_node", "My second node"), Some("label"))))

  println(graph.edges)
  // Set(MyEdge(None,MyNode(My first node),MyNode(My second node)))
  // #custom_graph
}
