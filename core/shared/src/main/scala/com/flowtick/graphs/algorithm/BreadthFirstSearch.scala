package com.flowtick.graphs.algorithm

import com.flowtick.graphs.Graph

import scala.collection.mutable

class BreadthFirstSearch[V, N, M](
  initialNodes: Iterable[N],
  graph: Graph[V, N, M]) extends Traversal[N] {
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

          def addAdjacent(nodes: Iterator[N]): Unit = {
            for (next <- nodes) {
              if (!visited.getOrElse(next, false)) {
                queue.enqueue(next)
              } else {
                backtrackCallbacks.foreach(_.apply(node))
              }
            }
          }
          addAdjacent(graph.successors(node))
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
