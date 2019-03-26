package com.flowtick.graphs.algorithm

import com.flowtick.graphs._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class DijkstraShortestPath[G[_, _, _], V, N, M](graph: G[V, N, M])(implicit
  graphType: Graph[G],
  numeric: Numeric[V]) {

  /**
   * determine the shortest path from start to end,
   * works only for positive weight values
   *
   * @param start the start node
   * @param end the end node
   * @return Some list of node ids with the shortest path, None if there is no path from start to end
   */
  def shortestPath(start: N, end: N): Iterable[Edge[V, N]] = {
    val distanceMap = mutable.Map.empty[N, Double]
    val predecessorMap = mutable.Map.empty[N, (N, Edge[V, N])]

    implicit val nodePriority: Ordering[N] = new Ordering[N] {
      override def compare(x: N, y: N): Int = {
        distanceMap(x).compareTo(distanceMap(y))
      }
    }.reverse

    val queue = mutable.PriorityQueue.empty[N]

    graphType.nodes(graph).foreach { node =>
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
        graphType.outgoing(graph).getOrElse(current, Iterable.empty).foreach { edge =>
          val weight = numeric.toDouble(edge.value)
          val newDist = currentDistance + weight

          Some(edge.tail).filter(newDist < distanceMap(_)).foreach { node =>
            distanceMap.put(node, newDist)
            predecessorMap.put(node, (current, edge))
            queue.enqueue(node)
          }

          /**
           * since we can not update the distance in queue, we mark the current one as done
           */
          distanceMap.put(current, Double.NaN)
        }
      }
    }

    if (predecessorMap.get(end).nonEmpty) {
      val predecessors = mutable.Stack[(N, Edge[V, N])]()
      val predecessorList = ListBuffer.empty[Edge[V, N]]
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
