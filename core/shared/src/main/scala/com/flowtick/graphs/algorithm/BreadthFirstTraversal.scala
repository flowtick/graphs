package com.flowtick.graphs.algorithm

import com.flowtick.graphs.{Graph, Node}
import com.flowtick.graphs.algorithm.Traversal.Step

class BreadthFirstTraversal[E, N](initialNodes: Iterable[String],
                                  graph: Graph[E, N]) extends Traversal[TraversalEvent[Step[E, N]]] {
  override def run: Iterable[TraversalEvent[Step[E, N]]] =
    Traversal.nodes(graph)(TraversalState.queue[Step[E, N], Node[N]](initialNodes.view.flatMap(node => graph.findNode(node).map(Step[E, N](_)))))
}