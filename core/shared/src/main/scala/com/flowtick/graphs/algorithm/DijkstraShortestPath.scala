package com.flowtick.graphs.algorithm

import com.flowtick.graphs._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class DijkstraShortestPath[M, E, N](graph: Graph[M, E, N])
                                           (implicit label: Labeled[Edge[E, N], E],
                                                     numeric: Numeric[E]) {

  /**
   * determine the shortest path from start to end,
   * works only for positive weight values
   *
   * @param start the start node
   * @param end the end node
   * @return Some list of node ids with the shortest path, None if there is no path from start to end
   */
  def shortestPath(start: N, end: N): Iterable[Edge[E, N]] = {
    val distanceMap = mutable.Map.empty[N, Double]
    val predecessorMap = mutable.Map.empty[N, (N, Edge[E, N])]

    implicit val nodePriority: Ordering[N] = new Ordering[N] {
      override def compare(x: N, y: N): Int = {
        distanceMap(x).compareTo(distanceMap(y))
      }
    }.reverse

    val queue = mutable.PriorityQueue.empty[N]

    graph.contexts.keys.foreach { node =>
      if (node == start) {
        distanceMap.put(start, 0)
      } else {
        distanceMap.put(node, Double.PositiveInfinity)
      }

      queue.enqueue(node)
    }

    while (queue.nonEmpty) {
      val current = queue.dequeue()
      val currentDistance: Double = distanceMap(current)
      if (currentDistance != Double.NaN) {
          graph.outgoing(current).foreach { edge =>
            val weight = numeric.toDouble(label(edge))
            val newDist = currentDistance + weight

            if(newDist < distanceMap(edge.to)) {
              distanceMap.put(edge.to, newDist)
              predecessorMap.put(edge.to, (current, edge))
              queue.enqueue(edge.to)
            }

            /**
             * since we can not update the distance in queue, we mark the current one as done
             */
            distanceMap.put(current, Double.NaN)
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
