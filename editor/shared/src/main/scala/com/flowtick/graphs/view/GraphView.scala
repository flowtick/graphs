package com.flowtick.graphs.view

import com.flowtick.graphs.{ Edge, Graph, Identifiable, Labeled }
import com.flowtick.graphs.layout.GraphLayout

trait GraphView[Container] {
  def createView[N, E](
    container: Container,
    graphLayout: GraphLayout,
    graph: Graph[N, E])(implicit nodeId: Identifiable[N], edgeLabel: Labeled[E, String])
}
