package examples

import com.flowtick.graphs.Graph
import com.flowtick.graphs.algorithm._
import com.flowtick.graphs.defaults._

object DijkstraGraph {
  // example taken from https://de.wikipedia.org/wiki/Dijkstra-Algorithmus
  // #cities
  val cities: Graph[Int, String] = Graph.fromEdges(Set(
    "Frankfurt" --> (85, "Mannheim"),
    "Frankfurt" --> (217, "Wuerzburg"),
    "Frankfurt" --> (173, "Kassel"),
    "Mannheim" --> (80, "Karlsruhe"),
    "Wuerzburg" --> (186, "Erfurt"),
    "Wuerzburg" --> (103, "Nuernberg"),
    "Stuttgart" --> (183, "Nuernberg"),
    "Kassel" --> (502, "Muenchen"),
    "Nuernberg" --> (167, "Muenchen"),
    "Karlsruhe" --> (250, "Augsburg"),
    "Augsburg" --> (84, "Muenchen")))
  // #cities
}

trait DijkstraExample {
  println(DijkstraGraph.cities.dijkstra.shortestPath("Frankfurt", "Muenchen"))
  // ListBuffer(Frankfurt --> Wuerzburg[217], Wuerzburg --> Nuernberg[103], Nuernberg --> Muenchen[167])
}
