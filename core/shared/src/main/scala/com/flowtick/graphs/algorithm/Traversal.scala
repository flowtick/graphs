package com.flowtick.graphs.algorithm

import com.flowtick.graphs.{Edge, Graph, Node}
import com.flowtick.graphs.algorithm.Traversal.Marker

import scala.collection.compat.immutable.LazyList
import scala.collection.immutable.Queue

sealed trait TraversalEvent[T] {
  def state: T
}

final case class Visited[S](state: S) extends TraversalEvent[S]
final case class Completed[S](state: S, backtrack: Option[S] = None) extends TraversalEvent[S]
final case class Backtrack[S](state: S) extends TraversalEvent[S]
final case class Skipped[S](state: S) extends TraversalEvent[S]

trait Traversal[E] {
  def run: Iterable[E]
}

object Traversal {
  sealed trait Marker[+T]

  private case object MarkerVisited extends Marker[Nothing]
  private case class MarkerComplete[S](backtrack: Option[S]) extends Marker[S]

  final case class Step[E, N](node: Node[N], edge: Option[Edge[E]] = None)

  def nodes[E, N](graph: Graph[E, N])(initial: TraversalState[Step[E, N], Node[N]]): Iterable[TraversalEvent[Step[E, N]]] =
    LazyList.unfold[TraversalEvent[Step[E, N]], TraversalState[Step[E, N], Node[N]]](initial) { lastState =>
      val (nodeStepOpt, currentState) = lastState.next

      nodeStepOpt.map { nodeStep =>
        currentState.getMarker(nodeStep.node) match {
          case None =>
            val visited = currentState.mark(nodeStep.node, MarkerVisited).add(nodeStep) // re-add to trigger completion
            val withNextNodes = graph.outgoing(nodeStep.node.id).foldLeft(visited) {
              case (nextState, edge) =>
                val continue = for {
                  nextNode <- graph.findNode(edge.to)
                  nextStep = Step(nextNode, Some(edge))
                } yield {
                  if (nextState.getMarker(nextStep.node).isDefined) {
                    nextState.mark(nextStep.node, MarkerComplete(Some(Step(nodeStep.node, Some(edge))))) // backtracking
                  } else nextState.add(nextStep)
                }

                continue.getOrElse(nextState)
            }
            ((Visited(nodeStep), withNextNodes))

          case Some(MarkerVisited) => ((Completed(nodeStep), currentState.mark(nodeStep.node, MarkerComplete(None))))
          case Some(MarkerComplete(backtrack)) if backtrack.isDefined => ((Completed(nodeStep, backtrack), currentState))
          case Some(MarkerComplete(None)) => ((Skipped(nodeStep)), currentState) // node appeared twice in state (was part of the inital state and the current traversal)
        }
      }      
    }
}

trait TraversalState[S, Id] {
  def next: (Option[S], TraversalState[S, Id])
  def add(state: S): TraversalState[S, Id]

  def mark(state: Id, marker: Traversal.Marker[S]): TraversalState[S, Id]
  def getMarker(state: Id): Option[Marker[S]]
}

object TraversalState {
  def stack[S, Id](nodes: Iterable[S]): TraversalState[S, Id] =
    StackBasedState[S, Id](nodes.toList)

  def queue[S, Id](nodes: Iterable[S]): TraversalState[S, Id] =
    QueueBasedState[S, Id](nodes.foldLeft(Queue.empty[S])((queue, node) => queue.enqueue(node)))
}

private final case class StackBasedState[S, Id](stack: List[S], markers: Map[Id, Traversal.Marker[S]] = Map.empty[Id, Traversal.Marker[S]]) extends TraversalState[S, Id] {
  override def mark(state: Id, marker: Traversal.Marker[S]): TraversalState[S, Id] =
    copy(markers = markers + (state -> marker))
  override def getMarker(state: Id): Option[Marker[S]] =
    markers.get(state)

  override def next: (Option[S], TraversalState[S, Id]) =
    (stack.headOption, copy(stack = if (stack.isEmpty) List.empty else stack.tail))
  override def add(nodeState: S): TraversalState[S, Id] =
    copy(stack = nodeState +: stack)
}

private final case class QueueBasedState[S, Id](queue: Queue[S], markers: Map[Id, Traversal.Marker[S]] = Map.empty[Id, Traversal.Marker[S]]) extends TraversalState[S, Id] {
  override def mark(state: Id, marker: Traversal.Marker[S]): TraversalState[S, Id] =
    copy(markers = markers + (state -> marker))
  override def getMarker(state: Id): Option[Marker[S]] =
    markers.get(state)

  override def next: (Option[S], TraversalState[S, Id]) =
    queue.dequeueOption.map {
      case (node, remaining) => (Some(node), copy(queue = remaining))
    }.getOrElse((None, this))
    
  override def add(nodeState: S): TraversalState[S, Id] =
    copy(queue = queue.enqueue(nodeState))
}