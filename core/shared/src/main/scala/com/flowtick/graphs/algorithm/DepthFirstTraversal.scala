package com.flowtick.graphs.algorithm

import com.flowtick.graphs.Graph
import com.flowtick.graphs.algorithm.Traversal.Step

class DepthFirstTraversal[E, N](
    initialNodes: Iterable[String],
    graph: Graph[E, N]
) extends StepTraversal[E, N] {
  override def run: Iterable[TraversalEvent[Step[E, N]]] =
    Traversal.nodes(graph)(initialNodes.view.flatMap(graph.findNode))(
      TraversalState.stack
    )
}
