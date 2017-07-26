package com.flowtick.graphs.algorithm

import com.flowtick.graphs.defaults._
import org.scalatest.{ FlatSpec, Matchers }

class DijkstraShortestPathSpec extends FlatSpec with Matchers {
  "Dijkstras algorithm" should "get the shortest path" in {

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

    g.shortestPath(n("Frankfurt"), n("Muenchen")).getOrElse(List()).map(_.id) should be(
      List("Frankfurt", "Wuerzburg", "Nuernberg", "Muenchen")
    )
  }
}
