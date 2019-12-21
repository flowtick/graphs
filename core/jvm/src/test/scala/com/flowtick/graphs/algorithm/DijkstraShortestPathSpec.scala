package com.flowtick.graphs.algorithm

import com.flowtick.graphs._
import com.flowtick.graphs.defaults._
import org.scalatest.{ FlatSpec, Matchers }

class DijkstraShortestPathSpec extends FlatSpec with Matchers {
  "Dijkstras algorithm" should "get the shortest path" in {

    // example taken from https://de.wikipedia.org/wiki/Dijkstra-Algorithmus
    val g = Graph.fromEdges(Seq(
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

    val pathFrankfurtMuenchen = g.shortestPath("Frankfurt", "Muenchen")
    pathFrankfurtMuenchen.headOption match {
      case Some(firstEdge) => firstEdge.head should be("Frankfurt")
      case None => fail("there should be path")
    }
    pathFrankfurtMuenchen.map(_.tail) should be(List("Wuerzburg", "Nuernberg", "Muenchen"))

    val pathFrankfurtErfurt = g.shortestPath("Frankfurt", "Erfurt")
    pathFrankfurtErfurt.headOption match {
      case Some(firstEdge) => firstEdge.head should be("Frankfurt")
      case None => fail("there should be path")
    }
    pathFrankfurtErfurt.map(_.tail) should be(List("Wuerzburg", "Erfurt"))
  }
}
