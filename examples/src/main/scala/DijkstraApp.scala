import com.flowtick.graphs.algorithm._
import com.flowtick.graphs.defaults._

object DijkstraApp extends App {
  // example taken from https://de.wikipedia.org/wiki/Dijkstra-Algorithmus
  val g = DefaultGraph.weighted { implicit g =>
    n("Frankfurt") ~ (85, n("Mannheim"))
    n("Frankfurt") ~ (217, n("Wuerzburg"))
    n("Frankfurt") ~ (173, n("Kassel"))
    n("Mannheim") ~ (80, n("Karlsruhe"))
    n("Wuerzburg") ~ (186, n("Erfurt"))
    n("Wuerzburg") ~ (103, n("Nuernberg"))
    n("Stuttgart") ~ (183, n("Nuernberg"))
    n("Kassel") ~ (502, n("Muenchen"))
    n("Nuernberg") ~ (167, n("Muenchen"))
    n("Karlsruhe") ~ (250, n("Augsburg"))
    n("Augsburg") ~ (84, n("Muenchen"))
  }

  println(g.shortestPath(start = n("Frankfurt"), end = n("Muenchen")))

  // Some(List(DefaultNode(Frankfurt), DefaultNode(Wuerzburg), DefaultNode(Nuernberg), DefaultNode(Muenchen)))
}
