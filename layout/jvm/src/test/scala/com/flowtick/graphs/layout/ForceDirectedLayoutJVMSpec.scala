package com.flowtick.graphs.layout

import com.flowtick.graphs.Graph
import com.flowtick.graphs.defaults._
import com.flowtick.graphs.defaults.label._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ForceDirectedLayoutJVMSpec extends AnyFlatSpec with Matchers with ScalaFutures {
  "Force Directed layout" should "layout plants graph" in {
    val plants = Graph.fromEdges[Unit, String](
      Set(
        "Norway Spruce" --> "Sicilian Fir",
        "Sicilian Fir" --> "Sumatran Pine",
        "Sicilian Fir" --> "Japanese Larch",
        "Norway Spruce" --> "Japanese Larch",
        "Norway Spruce" --> "Giant Sequoia"
      )
    )

    val config = GraphLayoutConfiguration(seed = Some(42))
    ForceDirectedLayout.layout(plants, config).futureValue should be(
      GraphLayout(
        nodes = Map(
          "Sumatran Pine" -> DefaultGeometry(29.0, 178.0, 80.0, 40.0),
          "Giant Sequoia" -> DefaultGeometry(198.0, 85.0, 80.0, 40.0),
          "Sicilian Fir" -> DefaultGeometry(118.0, 0.0, 80.0, 40.0),
          "Norway Spruce" -> DefaultGeometry(170.0, 204.0, 80.0, 40.0),
          "Japanese Larch" -> DefaultGeometry(0.0, 34.0, 80.0, 40.0)
        ),
        edges = Map()
      )
    )
  }
}
