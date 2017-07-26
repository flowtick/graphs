import com.flowtick.graphs.algorithm._
import com.flowtick.graphs.defaults._

object BfsApp extends App {
  val graph = DefaultGraph.create { implicit graph =>
    n("A") ~> n("D")
    n("A") ~> n("C")
    n("A") ~> n("B")
    n("B") ~> n("E")
    n("B") ~> n("F")
    n("B") ~> n("G")
    n("E") ~> n("H")
  }

  println(graph.bfs.traverse(startNode = Some(n("A"))).run)
  // List(DefaultNode(A), DefaultNode(D), DefaultNode(C), DefaultNode(B), DefaultNode(E), DefaultNode(F), DefaultNode(G), DefaultNode(H))
}
