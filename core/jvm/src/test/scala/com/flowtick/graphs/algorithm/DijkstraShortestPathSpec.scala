package com.flowtick.graphs.algorithm

import com.flowtick.graphs.defaults._
import com.flowtick.graphs.defaults.undirected._

import org.scalatest.{ FlatSpec, Matchers }

class DijkstraShortestPathSpec extends FlatSpec with Matchers {
  "Dijkstras algorithm" should "get the shortest path" in {

    // example taken from https://de.wikipedia.org/wiki/Dijkstra-Algorithmus
    val g = DefaultGraph.weighted[DefaultEdge[DefaultNode], DefaultNode, Int](Seq(
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

    g.shortestPath(n("Frankfurt"), n("Muenchen")).getOrElse(List()).map(_.id) should be(
      List("Frankfurt", "Wuerzburg", "Nuernberg", "Muenchen"))
  }
}
