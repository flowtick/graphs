import com.flowtick.graphs.Graph
import com.flowtick.graphs.defaults._
import com.flowtick.graphs.defaults.directed._
import com.flowtick.graphs.algorithm._

object TopologicalSortingApp extends App {
  lazy val graph = DefaultGraph.create(Seq(
    n("A") -> n("B"),
    n("B") -> n("C"),
    n("D") -> n("A")))

  lazy val clothingDependencies: Graph[DefaultNode, DirectedEdge[DefaultNode]] = DefaultGraph.create(Seq(
    n("Underpants") -> n("Pants"),
    n("Pants") -> n("Coat"),
    n("Pullover") -> n("Coat"),
    n("Undershirt") -> n("Pullover"),
    n("Pants") -> n("Shoes"),
    n("Socks") -> n("Shoes")))

  println(graph.topologicalSort)
  // List(DefaultNode(D), DefaultNode(A), DefaultNode(B), DefaultNode(C))

  println(clothingDependencies.topologicalSort)
  // List(DefaultNode(Socks), DefaultNode(Undershirt), DefaultNode(Pullover), DefaultNode(Underpants), DefaultNode(Pants), DefaultNode(Shoes), DefaultNode(Coat))
}
