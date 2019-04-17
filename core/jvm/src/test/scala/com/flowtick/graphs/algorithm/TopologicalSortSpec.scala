package com.flowtick.graphs.algorithm

import com.flowtick.graphs.Graph
import com.flowtick.graphs.defaults._
import org.scalatest.{ FlatSpec, Matchers }

class TopologicalSortSpec extends FlatSpec with Matchers {
  "Topological sort" should "sort dependent nodes" in {
    // https://de.wikipedia.org/wiki/Topologische_Sortierung#Beispiel:_Anziehreihenfolge_von_Kleidungsst.C3.BCcken
    val clothes = Graph.from(Seq(
      n("Unterhose") --> n("Hose"),
      n("Hose") --> n("Mantel"),
      n("Pullover") --> n("Mantel"),
      n("Unterhemd") --> n("Pullover"),
      n("Hose") --> n("Schuhe"),
      n("Socken") --> n("Schuhe")))

    clothes.topologicalSort should equal(
      List("Unterhemd", "Socken", "Pullover", "Unterhose", "Hose", "Mantel", "Schuhe"))
  }
}
