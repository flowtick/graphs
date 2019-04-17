import com.flowtick.graphs.defaults._
import com.flowtick.graphs.defaults.directed._
import com.flowtick.graphs.algorithm._

object DijkstraGraph {
  // example taken from https://de.wikipedia.org/wiki/Dijkstra-Algorithmus
  // #cities
  val cities = Graph.from(Set(
    n("Frankfurt") --> (85, n("Mannheim")),
    n("Frankfurt") --> (217, n("Wuerzburg")),
    n("Frankfurt") --> (173, n("Kassel")),
    n("Mannheim") --> (80, n("Karlsruhe")),
    n("Wuerzburg") --> (186, n("Erfurt")),
    n("Wuerzburg") --> (103, n("Nuernberg")),
    n("Stuttgart") --> (183, n("Nuernberg")),
    n("Kassel") --> (502, n("Muenchen")),
    n("Nuernberg") --> (167, n("Muenchen")),
    n("Karlsruhe") --> (250, n("Augsburg")),
    n("Augsburg") --> (84, n("Muenchen"))))
  // #cities
}

trait DijkstraExample {
  println(DijkstraGraph.cities.shortestPath("Frankfurt", "Muenchen"))
  // ListBuffer(Frankfurt --> Wuerzburg[217], Wuerzburg --> Nuernberg[103], Nuernberg --> Muenchen[167])
}
