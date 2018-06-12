import com.flowtick.graphs.algorithm._
import com.flowtick.graphs.defaults._
import com.flowtick.graphs.defaults.undirected._

object DijkstraGraph {
  // example taken from https://de.wikipedia.org/wiki/Dijkstra-Algorithmus
  // #cities
  val cities = DefaultGraph.weighted[DefaultEdge[DefaultNode], DefaultNode, Int](Seq(
    (85, n("Frankfurt") -> n("Mannheim")),
    (217, n("Frankfurt") -> n("Wuerzburg")),
    (173, n("Frankfurt") -> n("Kassel")),
    (80, n("Mannheim") -> n("Karlsruhe")),
    (186, n("Wuerzburg") -> n("Erfurt")),
    (103, n("Wuerzburg") -> n("Nuernberg")),
    (183, n("Stuttgart") -> n("Nuernberg")),
    (502, n("Kassel") -> n("Muenchen")),
    (167, n("Nuernberg") -> n("Muenchen")),
    (250, n("Karlsruhe") -> n("Augsburg")),
    (84, n("Augsburg") -> n("Muenchen"))))
  // #cities
}

object DijkstraApp extends App {
  println(DijkstraGraph.cities.shortestPath(start = n("Frankfurt"), end = n("Muenchen")).flatMap(_.left))
  // Some(List(DefaultNode(Frankfurt), DefaultNode(Wuerzburg), DefaultNode(Nuernberg), DefaultNode(Muenchen)))
}
