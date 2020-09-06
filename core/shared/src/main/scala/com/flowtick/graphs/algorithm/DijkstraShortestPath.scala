package com.flowtick.graphs.algorithm

import com.flowtick.graphs._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class DijkstraShortestPath[M, E, N](graph: Graph[E, N])
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
  def shortestPath(start: String, end: String): Iterable[Edge[E, N]] = {
    val distanceMap = mutable.Map.empty[String, Double]
    val predecessorMap = mutable.Map.empty[String, (Node[N], Edge[E, N])]

    implicit val nodePriority: Ordering[Node[N]] = new Ordering[Node[N]] {
      override def compare(x: Node[N], y: Node[N]): Int = {
        distanceMap(x.id).compareTo(distanceMap(y.id))
      }
    }.reverse

    val queue = mutable.PriorityQueue.empty[Node[N]]

    graph.nodes.foreach { node =>
      if (node.id == start) {
        distanceMap.put(start, 0)
      } else {
        distanceMap.put(node.id, Double.PositiveInfinity)
      }

      queue.enqueue(node)
    }

    while (queue.nonEmpty) {
      val current = queue.dequeue()
      val currentDistance: Double = distanceMap(current.id)
      if (currentDistance != Double.NaN) {
          graph.outgoing(current.id).foreach { edge =>
            val weight = numeric.toDouble(label(edge))
            val newDist = currentDistance + weight

            if(newDist < distanceMap(edge.to.id)) {
              distanceMap.put(edge.to.id, newDist)
              predecessorMap.put(edge.to.id, (current, edge))
              graph.findNode(edge.to.id).foreach(queue.enqueue(_))
            }

            /**
             * since we can not update the distance in queue, we mark the current one as done
             */
            distanceMap.put(current.id, Double.NaN)
          }

      }
    }

    if (predecessorMap.contains(end)) {
      val predecessors = mutable.Stack[(Node[N], Edge[E, N])]()
      val predecessorList = ListBuffer.empty[Edge[E, N]]
      predecessorMap.get(end).foreach(predecessors.push)

      while (predecessors.nonEmpty) {
        val currentPredecessor = predecessors.pop()
        predecessorList.prepend(currentPredecessor._2)
        predecessorMap.get(currentPredecessor._1.id).foreach(predecessors.push)
      }

      predecessorList
    } else List.empty
  }
}
