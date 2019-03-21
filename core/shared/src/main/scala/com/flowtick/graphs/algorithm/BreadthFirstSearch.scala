package com.flowtick.graphs.algorithm

import com.flowtick.graphs.{ EdgeType, Graph }

import scala.collection.mutable

class BreadthFirstSearch[G[_, _, _], E[_, _], V, N, M](
  initialNodes: Iterable[N],
  graph: G[E[V, N], N, M])(implicit graphType: Graph[G], edgeType: EdgeType[E]) extends Traversal[N] {
  override def run: Seq[N] = {
    val visited = mutable.Map[N, Boolean]()
    val visitedList = mutable.ListBuffer[N]()
    val queue = new mutable.Queue[N]()

    def traverse: Seq[N] = {
      while (queue.nonEmpty) {
        val node = queue.dequeue()
        val alreadyVisited = visited.put(node, true)
        if (alreadyVisited.isEmpty) {
          visitCallbacks.foreach(_.apply(node))
          visitedList += node

          def addAdjacent(nodes: Iterable[N]): Unit = {
            for (next <- nodes) {
              if (!visited.getOrElse(next, false)) {
                queue.enqueue(next)
              } else {
                backtrackCallbacks.foreach(_.apply(node))
              }
            }
          }
          addAdjacent(graphType.successors(node, graph))
          queue.enqueue(node)
        } else if (alreadyVisited.getOrElse(false)) {
          completeCallbacks.foreach(_.apply(node))
        }
      }
      visitedList.toList
    }

    initialNodes.foreach(queue.enqueue(_))
    traverse
  }
}
