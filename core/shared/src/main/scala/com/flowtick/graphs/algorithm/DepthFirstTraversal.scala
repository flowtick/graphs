package com.flowtick.graphs.algorithm

import com.flowtick.graphs.Graph
import com.flowtick.graphs.algorithm.Traversal.Step

class DepthFirstTraversal[E, N](initialNodes: Iterable[String],
                                graph: Graph[E, N]) extends Traversal[TraversalEvent[Step[E, N]]] {
  override def run: Iterable[TraversalEvent[Step[E, N]]] =
    Traversal.nodes(graph)(TraversalState.stack(initialNodes.view.flatMap(graph.findNode(_).map(Step[E, N](_)))))
}
