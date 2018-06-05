import com.flowtick.graphs.defaults._
import com.flowtick.graphs.defaults.directed._

object SimpleGraphApp extends App {
  val graph = DefaultGraph.create(Seq(
    n("A") -> n("B"),
    n("B") -> n("C"),
    n("D") -> n("A")))

  println(graph.edges)
  // Set(DefaultDirectedEdge(None,DefaultNode(B),DefaultNode(C)), DefaultDirectedEdge(None,DefaultNode(D),DefaultNode(A)), DefaultDirectedEdge(None,DefaultNode(A),DefaultNode(B)))
}
