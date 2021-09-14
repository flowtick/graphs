package examples

import com.flowtick.graphs._
import com.flowtick.graphs.algorithm._
import com.flowtick.graphs.defaults._

trait DfsExample {
  val graph = Graph.fromEdges(
    Set(
      "1" --> "2",
      "1" --> "9",
      "2" --> "6",
      "2" --> "3",
      "3" --> "5",
      "3" --> "4",
      "6" --> "7",
      "6" --> "8"
    )
  )

  println(graph.dfs("1").run)
  // List(1, 9, 2, 6, 8, 7, 3, 5, 4)
}
