package com.flowtick.graphs.algorithm

import com.flowtick.graphs.Graph
import com.flowtick.graphs.defaults._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TopologicalSortSpec extends AnyFlatSpec with Matchers {
  "Topological sort" should "sort dependent nodes" in {
    // https://de.wikipedia.org/wiki/Topologische_Sortierung#Beispiel:_Anziehreihenfolge_von_Kleidungsst.C3.BCcken
    val clothes = Graph.fromEdges(Seq(
      "Unterhose" --> "Hose",
      "Hose" --> "Mantel",
      "Pullover" --> "Mantel",
      "Unterhemd" --> "Pullover",
      "Hose" --> "Schuhe",
      "Socken" --> "Schuhe"))

    val sorted = clothes.topologicalSort.map(_.id)

    clothes.edges.forall { edge =>
      sorted.indexOf(edge.from.value) < sorted.indexOf(edge.to.value)
    } should be(true)
  }
}
