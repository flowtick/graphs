import com.flowtick.graphs._
import com.flowtick.graphs.defaults._
import com.flowtick.graphs.algorithm._

object TopologicalSortingApp extends App {
  val graph = Graph.create[DefaultNode, Edge[DefaultNode]] { implicit graph =>
    n("A") ~> n("B")
    n("B") ~> n("C")
    n("D") ~> n("A")
  }

  println(graph.topologicalSort)
  // List(DefaultNode(D), DefaultNode(A), DefaultNode(B), DefaultNode(C))
}
