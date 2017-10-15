package com.flowtick.graphs.traversal

import com.flowtick.graphs._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class AdjacentNodesTraversal[N <: Node, E <: Edge[N]](
  startNode: Option[N] = None,
  graph: Graph[N, E],
  traversalBuffer: () => TraversalBuffer[N]) extends Traversal[N] {
  val visitCallbacks: ListBuffer[N => Any] = ListBuffer.empty
  val completeCallbacks: ListBuffer[N => Any] = ListBuffer.empty
  val backtrackCallbacks: ListBuffer[N => Any] = ListBuffer.empty

  override def run: List[N] = {
    val visited = mutable.Map[N, Boolean]()
    val visitedList = mutable.ListBuffer[N]()

    def traverse(buffer: TraversalBuffer[N]) = {
      while (buffer.nonEmpty) {
        val node = buffer.get
        val alreadyVisited = visited.put(node, true)
        if (alreadyVisited.isEmpty) {
          visitCallbacks.foreach(_.apply(node))
          visitedList += node
          buffer.addCurrent(node)

          def addAdjacent(edges: Iterable[E], extractNext: E => N) = {
            for (edge <- edges) {
              val next = extractNext(edge)
              if (!visited.getOrElse(next, false)) {
                buffer.addCurrent(next)
              } else {
                backtrackCallbacks.foreach(_.apply(node))
              }
            }
          }

          addAdjacent(graph.outgoing(node), n => n.target)

        } else if (alreadyVisited.getOrElse(false)) {
          completeCallbacks.foreach(_.apply(node))
        }
      }
    }

    val startNodes = if (startNode.nonEmpty) Set(startNode).flatten else graph.nodes

    startNodes.foreach(node => {
      if (visited.get(node).isEmpty) {
        val buffer = traversalBuffer()
        buffer.addCurrent(node)
        traverse(buffer)
      }
    })

    visitedList.toList
  }

  override def onVisit(f: N => Any): Traversal[N] = {
    visitCallbacks += f
    this
  }

  override def onComplete(f: N => Any): Traversal[N] = {
    completeCallbacks += f
    this
  }

  override def onBacktrack(f: N => Any): Traversal[N] = {
    backtrackCallbacks += f
    this
  }
}
