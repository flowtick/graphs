package examples

import com.flowtick.graphs._
import com.flowtick.graphs.algorithm._
import com.flowtick.graphs.defaults._

trait BfsExample {
  val graph = Graph.fromEdges(
    Set(
      "A" --> "D",
      "A" --> "C",
      "A" --> "B",
      "B" --> "E",
      "B" --> "F",
      "B" --> "G",
      "E" --> "H"
    )
  )

  println(graph.bfs("A").run)
  // List(A, B, C, D, E, F, G, H)
}
