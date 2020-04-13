package com.flowtick.graphs.algorithm

import com.flowtick.graphs.Graph
import com.flowtick.graphs.defaults._
import org.scalatest.{ FlatSpec, Matchers }

class TopologicalSortSpec extends FlatSpec with Matchers {
  "Topological sort" should "sort dependent nodes" in {
    // https://de.wikipedia.org/wiki/Topologische_Sortierung#Beispiel:_Anziehreihenfolge_von_Kleidungsst.C3.BCcken
    val clothes = Graph.fromEdges(Seq(
      "Unterhose" --> "Hose",
      "Hose" --> "Mantel",
      "Pullover" --> "Mantel",
      "Unterhemd" --> "Pullover",
      "Hose" --> "Schuhe",
      "Socken" --> "Schuhe"))

    // FIXME: topological sort result is different (but still valid) comparing 2.12 and 2.13
    val validSortings = List(
      List("Unterhemd", "Unterhose", "Hose", "Socken", "Schuhe", "Pullover", "Mantel"),
      List("Unterhemd", "Unterhose", "Pullover", "Hose", "Mantel", "Socken", "Schuhe"),
      List("Socken", "Unterhemd", "Pullover", "Unterhose", "Hose", "Schuhe", "Mantel"),
      List("Unterhemd", "Socken", "Unterhose", "Pullover", "Hose", "Schuhe", "Mantel")
    )

    validSortings should contain(clothes.topologicalSort)
  }
}
