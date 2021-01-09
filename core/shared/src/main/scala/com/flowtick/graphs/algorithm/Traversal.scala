package com.flowtick.graphs.algorithm

import com.flowtick.graphs.{Graph, Node}
import com.flowtick.graphs.algorithm.Traversal.Marker

import scala.collection.immutable.Queue

sealed trait TraversalStep[T] {
  def state: T
}

final case class Visited[S](state: S) extends TraversalStep[S]
final case class Completed[S](state: S) extends TraversalStep[S]
final case class Backtrack[S](state: S) extends TraversalStep[S]
final case class Skipped[S](state: S) extends TraversalStep[S]

trait Traversal[E] {
  def run: Iterable[E]
}

object Traversal {
  sealed trait Marker

  private case object MarkerVisited extends Marker
  private case object MarkerComplete extends Marker
  private case object MarkerBacktrack extends Marker

  def nodes[E, N](graph: Graph[E, N])(initial: TraversalState[Node[N]]): Iterable[TraversalStep[Node[N]]] =
    Iterable.unfold[TraversalStep[Node[N]], TraversalState[Node[N]]](initial) { lastState =>
      val (nodeOpt, currentState) = lastState.next

      nodeOpt.map { node =>
        currentState.getMarker(node) match {
          case None =>
            val visited = currentState.mark(node, MarkerVisited).add(node) // re-add to trigger completion
            val withNextNodes = graph.successors(node.id).foldLeft(visited) {
              case (nextState, successor) => 
                if (nextState.getMarker(successor).isDefined) {
                  nextState.mark(successor, MarkerBacktrack) // backtracking
                } else nextState.add(successor)
            }
            ((Visited(node), withNextNodes))
            
          case Some(MarkerVisited) => ((Completed(node), currentState.mark(node, MarkerComplete)))
          case Some(MarkerBacktrack) => ((Backtrack(node), currentState))
          case Some(MarkerComplete) => ((Skipped(node)), currentState) // node appeared twice in state (was part of the inital state and the current traversal)
        }
      }      
    }
}

trait TraversalState[S] {
  def next: (Option[S], TraversalState[S])
  def add(state: S): TraversalState[S]

  def mark(state: S, marker: Traversal.Marker): TraversalState[S]
  def getMarker(state: S): Option[Marker]
}

object TraversalState {
  def stack[N](nodes: Iterable[Node[N]]): TraversalState[Node[N]] =
    StackBasedState[Node[N]](nodes.toList)

  def queue[N](nodes: Iterable[Node[N]]): TraversalState[Node[N]] =
    QueueBasedState[Node[N]](Queue.from(nodes))
}

private final case class StackBasedState[S](stack: List[S], markers: Map[S, Traversal.Marker] = Map.empty[S, Traversal.Marker]) extends TraversalState[S] {
  override def mark(state: S, marker: Traversal.Marker): TraversalState[S] = copy(markers = markers + (state -> marker))
  override def getMarker(state: S): Option[Marker] = markers.get(state)

  override def next: (Option[S], TraversalState[S]) = (stack.headOption, copy(stack = if (stack.isEmpty) List.empty else stack.tail))
  override def add(nodeState: S): TraversalState[S] = copy(stack = nodeState +: stack)
}

private final case class QueueBasedState[S](queue: Queue[S], markers: Map[S, Traversal.Marker] = Map.empty[S, Traversal.Marker]) extends TraversalState[S] {
  override def mark(state: S, marker: Traversal.Marker): TraversalState[S] = copy(markers = markers + (state -> marker))
  override def getMarker(state: S): Option[Marker] = markers.get(state)

  override def next: (Option[S], TraversalState[S]) =
    queue.dequeueOption.map {
      case (node, remaining) => (Some(node), copy(queue = remaining))
    }.getOrElse((None, this))
    
  override def add(nodeState: S): TraversalState[S] = copy(queue = queue.enqueue(nodeState))
}