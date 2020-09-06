package com.flowtick.graphs.algorithm

import com.flowtick.graphs.{Graph, Node}

import scala.collection.mutable

class DepthFirstSearch[E, N](initialNodes: Iterable[String],
                             graph: Graph[E, N]) extends Traversal[Node[N]] {
  override def run: Seq[Node[N]] = {
    val visited = mutable.Map[Node[N], Boolean]()
    val visitedList = mutable.ListBuffer[Node[N]]()
    val stack = new mutable.Stack[(Node[N], Boolean)]()

    def traverse: Seq[Node[N]] = {
      while (stack.nonEmpty) {
        val (node, completed) = stack.pop()
        val alreadyVisited = visited.put(node, true)
        if (alreadyVisited.isEmpty) {
          visitCallbacks.foreach(_.apply(node))
          visitedList += node

          // to recognize that we completed that node, this will trigger the completion callback branch
          stack.push((node, true))

          def addAdjacent(nodes: Iterable[Node[N]]): Unit =
            nodes.foreach { next =>
              if (!visited.getOrElse(next, false)) {
                stack.push((next, false))
              } else {
                backtrackCallbacks.foreach(_.apply(node))
              }
            }

          addAdjacent(graph.successors(node.id))
        } else if (completed) {
          completeCallbacks.foreach(_.apply(node))
        }
      }
      visitedList.toList
    }

    stack.pushAll(initialNodes.flatMap(graph.findNode).map(node => (node, false)))
    traverse
  }
}
