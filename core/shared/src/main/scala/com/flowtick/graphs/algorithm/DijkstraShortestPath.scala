package com.flowtick.graphs.algorithm

import com.flowtick.graphs._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class DijkstraShortestPath[T: Numeric, N <: Node, E <: WeightedEdge[T, N]](graph: Graph[N, WeightedEdge[T, N]]) {
  /**
   * determine the shortest path from start to end,
   * works only for positive weight values
   *
   * @param start the start node
   * @param end the end node
   * @return Some list of node ids with the shortest path, None if there is no path from start to end
   */
  def shortestPath(start: N, end: N): Option[List[N]] = {
    val numeric: Numeric[T] = implicitly[Numeric[T]]
    val distanceMap = mutable.Map.empty[N, Double]
    val predecessorMap = mutable.Map.empty[N, N]

    implicit val nodePriority = new Ordering[N] {
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
        val newDist = currentDistance + numeric.toDouble(edge.weight)
        if (newDist < distanceMap(edge.target)) {
          distanceMap.put(edge.target, newDist)
          predecessorMap.put(edge.target, current)
        }
      }
    }

    if (predecessorMap.get(end).nonEmpty) {
      val predecessors = mutable.Stack[N]()
      val predecessorList = ListBuffer.empty[N]
      predecessorMap.get(end).foreach(predecessors.push)

      while (predecessors.nonEmpty) {
        val currentPredecessor = predecessors.pop()
        predecessorList.prepend(currentPredecessor)
        predecessorMap.get(currentPredecessor).foreach(predecessors.push)
      }

      predecessorList.append(end)

      Some(predecessorList.toList)
    } else None
  }
}
