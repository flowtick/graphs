package com.flowtick.graphs.algorithm

import com.flowtick.graphs._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class DijkstraShortestPath[G[_, _, _], E[_, _], V, N, M](graph: G[E[V, N], N, M])(implicit
  graphType: Graph[G, E],
  edgeType: EdgeType[E],
  identifiable: Identifiable[N],
  numeric: Numeric[V]) {

  /**
   * determine the shortest path from start to end,
   * works only for positive weight values
   *
   * @param start the start node
   * @param end the end node
   * @return Some list of node ids with the shortest path, None if there is no path from start to end
   */
  def shortestPath(start: N, end: N): Iterable[E[V, N]] = {
    val distanceMap = mutable.Map.empty[N, Double]
    val predecessorMap = mutable.Map.empty[N, (N, E[V, N])]

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
        graphType.outgoing(current, graph).foreach { edge =>
          val weight = numeric.toDouble(edgeType.value(edge))
          val newDist = currentDistance + weight

          Some(edgeType.tail(edge)).filter(newDist < distanceMap(_)).foreach { node =>
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
      val predecessors = mutable.Stack[(N, E[V, N])]()
      val predecessorList = ListBuffer.empty[E[V, N]]
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
