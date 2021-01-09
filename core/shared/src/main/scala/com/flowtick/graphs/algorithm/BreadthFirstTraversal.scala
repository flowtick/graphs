package com.flowtick.graphs.algorithm

import com.flowtick.graphs.{Graph, Node}

class BreadthFirstTraversal[E, N](initialNodes: Iterable[String],
                                graph: Graph[E, N]) extends Traversal[TraversalStep[Node[N]]] {
  override def run: Iterable[TraversalStep[Node[N]]] =
    Traversal.nodes(graph)(TraversalState.queue(initialNodes.view.flatMap(graph.findNode)))
}