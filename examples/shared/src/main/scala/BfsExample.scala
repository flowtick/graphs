import com.flowtick.graphs.Graph
import com.flowtick.graphs.algorithm._
import com.flowtick.graphs.defaults._

trait BfsExample {
  val graph = Graph.from(Set(
    n("A") --> n("D"),
    n("A") --> n("C"),
    n("A") --> n("B"),
    n("B") --> n("E"),
    n("B") --> n("F"),
    n("B") --> n("G"),
    n("E") --> n("H")))

  println(graph.bfs("A").run)
  // List(A, B, C, D, E, F, G, H)
}
