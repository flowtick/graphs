package com.flowtick.graphs.algorithm

import com.flowtick.graphs._
import com.flowtick.graphs.defaults._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DijkstraShortestPathSpec extends AnyFlatSpec with Matchers {
  "Dijkstras algorithm" should "get the shortest path" in {

    // example taken from https://de.wikipedia.org/wiki/Dijkstra-Algorithmus
    val g = Graph.fromEdges(Seq(
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

    val pathFrankfurtMuenchen = g.dijkstra.shortestPath("Frankfurt", "Muenchen")
    pathFrankfurtMuenchen.headOption match {
      case Some(firstEdge) => firstEdge.from should be("Frankfurt")
      case None => fail("there should be path")
    }
    pathFrankfurtMuenchen.map(_.to) should be(List("Wuerzburg", "Nuernberg", "Muenchen"))

    val pathFrankfurtErfurt = g.dijkstra.shortestPath("Frankfurt", "Erfurt")
    pathFrankfurtErfurt.headOption match {
      case Some(firstEdge) => firstEdge.from should be("Frankfurt")
      case None => fail("there should be path")
    }
    pathFrankfurtErfurt.map(_.to) should be(List("Wuerzburg", "Erfurt"))
  }
}
