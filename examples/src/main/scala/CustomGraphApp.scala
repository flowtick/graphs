import com.flowtick.graphs._

object CustomGraphApp extends App {
  // #custom_graph
  case class MyNode(id: String, someCustomProperty: String)
  case class MyEdge(label: Option[String] = None, source: MyNode, target: MyNode)
  case class MyGraph(override val nodes: Set[MyNode], edges: Set[MyEdge])(implicit edge: Edge[MyEdge, MyNode]) extends AbstractGraph[MyNode, MyEdge]

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

    implicit def graphBuilder(implicit edge: Edge[MyEdge, MyNode]) = new GraphBuilder[MyNode, MyEdge, MyGraph, Unit] {
      override def create(edges: Set[MyEdge], nodes: Set[MyNode], graphParams: Unit): MyGraph = MyGraph(nodes, edges)
    }

    implicit def edge = new Edge[MyEdge, MyNode] {
      override def first(edge: MyEdge): MyNode = edge.source
      override def second(edge: MyEdge): Option[MyNode] = Some(edge.target)
    }

    def create = Graph.create[MyNode, MyEdge, MyGraph, ((MyNode, MyNode), Option[String]), Unit] _
  }

  val graph = MyGraph.create(
    Seq((MyNode("first_node", "My first node") -> MyNode("second_node", "My second node"), Some("label")))).apply()

  println(graph.edges)
  // Set(MyEdge(None,MyNode(My first node),MyNode(My second node)))
  // #custom_graph
}
