package com.flowtick.graphs.algorithm

import com.flowtick.graphs.Graph
import com.flowtick.graphs.algorithm.Traversal.Step

class BreadthFirstTraversal[E, N](
    initialNodes: Iterable[String],
    graph: Graph[E, N]
) extends Traversal[TraversalEvent[Step[E, N]]] {
  override def run: Iterable[TraversalEvent[Step[E, N]]] =
    Traversal.nodes(graph)(initialNodes.view.flatMap(graph.findNode))(
      TraversalState.queue
    )
}
