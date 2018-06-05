import com.flowtick.graphs.algorithm._
import com.flowtick.graphs.defaults._
import com.flowtick.graphs.defaults.directed._

object DfsApp extends App {
  val graph = DefaultGraph.create(Seq(
    n("1") -> n("2"),
    n("1") -> n("9"),

    n("2") -> n("6"),
    n("2") -> n("3"),

    n("3") -> n("5"),
    n("3") -> n("4"),

    n("6") -> n("7"),
    n("6") -> n("8")))

  println(graph.dfs(n("1")).run)
  // List(DefaultNode(1), DefaultNode(9), DefaultNode(2), DefaultNode(3), DefaultNode(4), DefaultNode(5), DefaultNode(6), DefaultNode(8), DefaultNode(7))
}
