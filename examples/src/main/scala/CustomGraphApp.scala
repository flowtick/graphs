import com.flowtick.graphs._

object CustomGraphApp extends App {
  // #custom_graph
  case class MyNode(someCustomProperty: String) extends Node
  case class MyEdge(label: Option[String] = None, source: MyNode, target: MyNode) extends DirectedEdge[MyNode]
  case class MyGraph(graphBuilder: GraphBuilder[MyNode, MyEdge]) extends AbstractGraph[MyNode, MyEdge](graphBuilder)

  class MyGraphBuilder extends GraphBuilder[MyNode, MyEdge] {
    override def build: MyGraph = new MyGraph(this)
  }

  object MyGraph {
    def create(block: MyGraphBuilder => Any): MyGraph = {
      val builder = new MyGraphBuilder
      block.apply(builder)
      builder.build
    }
  }

  val graph = MyGraph.create { implicit graph =>
    graph.addEdge(MyEdge(label = None, MyNode("My first node"), MyNode("My second node")))
  }

  println(graph.edges)
  // Set(MyEdge(None,MyNode(My first node),MyNode(My second node)))
  // #custom_graph

  // #custom_graph_builder
  implicit class MyNodeOps(n: MyNode) extends NodeOps[MyNode, MyEdge]
    with DirectedNodeOps[MyNode, MyEdge] {
    val node: MyNode = n

    override def ~>(target: MyNode)(implicit graphBuilder: GraphBuilder[MyNode, MyEdge]): MyNode = {
      graphBuilder.addEdge(MyEdge(None, node, target))
      target
    }
  }

  // allows

  MyGraph.create { implicit graph =>
    MyNode("My first node") ~> MyNode("My second node")
  }
  // #custom_graph_builder
}
