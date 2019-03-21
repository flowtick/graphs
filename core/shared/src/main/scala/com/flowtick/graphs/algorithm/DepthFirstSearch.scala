package com.flowtick.graphs.algorithm

import com.flowtick.graphs.{ EdgeType, Graph }

import scala.collection.mutable

class DepthFirstSearch[G[_, _, _], E[_, _], V, N, M](
  initialNodes: Iterable[N],
  graph: G[E[V, N], N, M])(implicit graphType: Graph[G], edgeType: EdgeType[E]) extends Traversal[N] {
  override def run: Seq[N] = {
    val visited = mutable.Map[N, Boolean]()
    val visitedList = mutable.ListBuffer[N]()
    val stack = new mutable.Stack[(N, Boolean)]()

    def traverse: Seq[N] = {
      while (stack.nonEmpty) {
        val (node, completed) = stack.pop()
        val alreadyVisited = visited.put(node, true)
        if (alreadyVisited.isEmpty) {
          visitCallbacks.foreach(_.apply(node))
          visitedList += node

          // to recognize that we completed that node, this will trigger the completion callback branch
          stack.push((node, true))

          def addAdjacent(nodes: Iterable[N]): Unit =
            nodes.foreach { next =>
              if (!visited.getOrElse(next, false)) {
                stack.push((next, false))
              } else {
                backtrackCallbacks.foreach(_.apply(node))
              }
            }

          addAdjacent(graphType.successors(node, graph))
        } else if (completed) {
          completeCallbacks.foreach(_.apply(node))
        }
      }
      visitedList.toList
    }

    stack.pushAll(initialNodes.map(node => (node, false)))
    traverse
  }
}
