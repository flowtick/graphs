package com.flowtick.graphs.algorithm

import com.flowtick.graphs._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class DijkstraShortestPath[T, N, E](graph: Graph[N, E]) {
  /**
   * determine the shortest path from start to end,
   * works only for positive weight values
   *
   * @param start the start node
   * @param end the end node
   * @return Some list of node ids with the shortest path, None if there is no path from start to end
   */
  def shortestPath(start: N, end: N)(implicit weight: Weighted[E, T], numeric: Numeric[T]): Iterable[Edge[E, N]] = {
    val distanceMap = mutable.Map.empty[N, Double]
    val predecessorMap = mutable.Map.empty[N, (N, Edge[E, N])]

    implicit val nodePriority: Ordering[N] = new Ordering[N] {
      override def compare(x: N, y: N): Int = -distanceMap(x).compare(distanceMap(y))
    }

    val queue = mutable.PriorityQueue.empty[N]

    distanceMap.put(start, 0)

    graph.nodes.foreach { node =>
      if (node != start) {
        distanceMap.put(node, Int.MaxValue)
      }
      queue.enqueue(node)
    }

    while (queue.nonEmpty) {
      val current = queue.dequeue()
      graph.outgoing(current).foreach { edge =>
        val currentDistance: Double = distanceMap(current)
        val newDist = currentDistance + numeric.toDouble(weight.value(edge.value))
        edge.successors.filter(newDist < distanceMap(_)).foreach { node =>
          distanceMap.put(node, newDist)
          predecessorMap.put(node, (current, edge))
        }
      }
    }

    if (predecessorMap.get(end).nonEmpty) {
      val predecessors = mutable.Stack[(N, Edge[E, N])]()
      val predecessorList = ListBuffer.empty[Edge[E, N]]
      predecessorMap.get(end).foreach(predecessors.push)

      while (predecessors.nonEmpty) {
        val currentPredecessor = predecessors.pop()
        predecessorList.prepend(currentPredecessor._2)
        predecessorMap.get(currentPredecessor._1).foreach(predecessors.push)
      }

      predecessorList
    } else List.empty
  }
}
