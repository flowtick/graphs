package com.flowtick.graphs.algorithm

import com.flowtick.graphs.{Graph, Node}

import scala.collection.mutable

class BreadthFirstSearch[E, N](initialNodes: Iterable[String],
                               graph: Graph[E, N]) extends Traversal[Node[N]] {
  override def run: Seq[Node[N]] = {
    val visited = mutable.Map[Node[N], Boolean]()
    val visitedList = mutable.ListBuffer[Node[N]]()
    val queue = new mutable.Queue[Node[N]]()

    def traverse: Seq[Node[N]] = {
      while (queue.nonEmpty) {
        val node = queue.dequeue()
        val alreadyVisited = visited.put(node, true)
        if (alreadyVisited.isEmpty) {
          visitCallbacks.foreach(_.apply(node))
          visitedList += node

          def addAdjacent(nodes: Iterable[Node[N]]): Unit = {
            for (next <- nodes) {
              if (!visited.getOrElse(next, false)) {
                queue.enqueue(next)
              } else {
                backtrackCallbacks.foreach(_.apply(node))
              }
            }
          }
          addAdjacent(graph.successors(node.id))
          queue.enqueue(node)
        } else if (alreadyVisited.getOrElse(false)) {
          completeCallbacks.foreach(_.apply(node))
        }
      }
      visitedList.toList
    }

    initialNodes.flatMap(graph.findNode).foreach(queue.enqueue(_))
    traverse
  }
}
