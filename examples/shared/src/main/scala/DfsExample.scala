import com.flowtick.graphs.algorithm._
import com.flowtick.graphs.defaults._
import com.flowtick.graphs.defaults.directed._

trait DfsExample {
  val graph = defaultGraph.from(Set(
    n("1") --> n("2"),
    n("1") --> n("9"),

    n("2") --> n("6"),
    n("2") --> n("3"),

    n("3") --> n("5"),
    n("3") --> n("4"),

    n("6") --> n("7"),
    n("6") --> n("8")))

  println(graph.dfs("1").run)
  // List(1, 9, 2, 6, 8, 7, 3, 5, 4)
}
