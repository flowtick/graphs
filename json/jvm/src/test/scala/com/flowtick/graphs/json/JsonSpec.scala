package com.flowtick.graphs.json

import com.flowtick.graphs.{ Edge, Graph, Identifiable }
import com.flowtick.graphs.defaults._
import com.flowtick.graphs.defaults.id._
import org.scalatest.FlatSpec

class JsonSpec extends FlatSpec {
  "JSON support" should "encode int graph" in {
    val json = ToJson(Graph.fromEdges(Set(
      n(1) --> n(2))))

    println(json)
  }
}
