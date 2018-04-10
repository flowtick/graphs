import com.flowtick.graphs.algorithm._
import com.flowtick.graphs.defaults._
import com.flowtick.graphs.defaults.directed._

object BfsApp extends App {
  val graph = DefaultGraph.create(Seq(
    n("A") -> n("D"),
    n("A") -> n("C"),
    n("A") -> n("B"),
    n("B") -> n("E"),
    n("B") -> n("F"),
    n("B") -> n("G"),
    n("E") -> n("H")))

  println(graph.bfs(n("A")).run)
  // List(DefaultNode(A), DefaultNode(D), DefaultNode(C), DefaultNode(B), DefaultNode(E), DefaultNode(F), DefaultNode(G), DefaultNode(H))
}
