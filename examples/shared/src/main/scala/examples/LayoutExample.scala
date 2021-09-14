package examples

import com.flowtick.graphs.layout.{GraphLayoutLike, GraphLayoutOps}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait LayoutExample {
  def layoutOps: GraphLayoutOps

  // #layout_simple
  import com.flowtick.graphs._
  import com.flowtick.graphs.defaults.label._

  lazy val graph: Graph[Int, String] = DijkstraGraph.cities
  lazy val layout: Future[GraphLayoutLike] = layoutOps.layout(graph)

  layout.onComplete(println)
  // #layout_simple
}
