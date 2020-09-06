import com.flowtick.graphs.layout.JGraphXLayouter

object LayoutExample extends App {
  import com.flowtick.graphs._
  import com.flowtick.graphs.defaults._
  import com.flowtick.graphs.defaults.label._

  val graph: Graph[Unit, String] = Graph.fromEdges(Set(
    "A" --> "B",
    "B" --> "C",
    "D" --> "A"))

  JGraphXLayouter.layout(graph)
}
