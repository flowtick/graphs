package com.flowtick.graphs.algorithm

import com.flowtick.graphs.{ Edge, Graph }

import scala.collection.mutable

class BreadthFirstSearch[N, E](initialNodes: Iterable[N], graph: Graph[N, E]) extends Traversal[N] {
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

          def addAdjacent(edges: Iterable[Edge[E, N]]): Unit = {
            for (edge <- edges) {
              edge.successors.foreach { next =>
                if (!visited.getOrElse(next, false)) {
                  queue.enqueue(next)
                } else {
                  backtrackCallbacks.foreach(_.apply(node))
                }
              }
            }
          }
          addAdjacent(graph.outgoing(node))
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
