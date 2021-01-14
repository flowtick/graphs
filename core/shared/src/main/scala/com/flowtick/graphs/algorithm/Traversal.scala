package com.flowtick.graphs.algorithm

import com.flowtick.graphs.{Edge, Graph, Node}
import com.flowtick.graphs.algorithm.Traversal.Marker

import scala.collection.compat.immutable.LazyList
import scala.collection.immutable.Queue

sealed trait TraversalEvent[T] {
  def state: T
}

final case class Visited[S](state: S) extends TraversalEvent[S]
final case class Completed[S](state: S, backtrack: Option[S] = None) extends TraversalEvent[S] {
  override def toString: String = s"Completed(state = $state, backtracking: ${backtrack.isDefined} ($backtrack))"
}
final case class Backtrack[S](state: S) extends TraversalEvent[S]
final case class Skipped[S](state: S) extends TraversalEvent[S]

trait Traversal[E] {
  def run: Iterable[E]
}

object Traversal {
  sealed trait Marker[+T]

  private case object MarkerVisited extends Marker[Nothing]
  private final case class MarkerComplete[S](backtrack: Option[S]) extends Marker[S]

  final case class Step[E, N](node: Node[N], edge: Option[Edge[E]] = None, depth: Option[Int] = None)

  def nodes[E, N](graph: Graph[E, N])(initialNodes: Iterable[Node[N]])(state: TraversalState[Step[E, N], Node[N]]): Iterable[TraversalEvent[Step[E, N]]] = {
    val initialState = initialNodes.foldLeft(state)((currentState, node) => currentState.add(Step(node, depth = Some(0))))

    LazyList.unfold[TraversalEvent[Step[E, N]], TraversalState[Step[E, N], Node[N]]](initialState) { lastState =>
      val (nodeStepOpt, currentState) = lastState.next

      nodeStepOpt.map { nodeStep =>
        currentState.getMarker(nodeStep.node) match {
          case None =>
            val withNextNodes = currentState.triggerCompletion { nextState =>
              val visited = nextState.mark(nodeStep.node, MarkerVisited)
              graph.outgoing(nodeStep.node.id).foldLeft(visited) {
                case (added, edge) =>
                  val continue = for {
                    nextNode <- graph.findNode(edge.to)
                    nextStep = Step(nextNode, Some(edge), depth = nodeStep.depth.map(_ + 1))
                  } yield {
                    if (added.getMarker(nextNode).contains(MarkerVisited)) {
                      added.mark(nextNode, MarkerComplete(Some(Step(nodeStep.node, Some(edge), nodeStep.depth)))) // backtracking
                    } else added.add(nextStep)
                  }

                  continue.getOrElse(added)
              }
            }(nodeStep)

            ((Visited(nodeStep), withNextNodes))

          case Some(MarkerVisited) => ((Completed(nodeStep), currentState.mark(nodeStep.node, MarkerComplete(None))))
          case Some(MarkerComplete(Some(backtrack))) => ((Completed(nodeStep, Some(backtrack)), currentState))
          case Some(MarkerComplete(None)) => ((Skipped(nodeStep)), currentState) // node appeared twice in state (was part of the inital state and the current traversal)
        }
      }      
    }
  }
}

trait TraversalState[S, Id] {
  def next: (Option[S], TraversalState[S, Id])
  def add(state: S): TraversalState[S, Id]

  def mark(state: Id, marker: Traversal.Marker[S]): TraversalState[S, Id]
  def getMarker(state: Id): Option[Marker[S]]

  /**
   * trigger the completion after the state returned
   * @param f transformation of the current state, should ne used to add next states
   * @param completionState
   * @return
   */
  def triggerCompletion(f: TraversalState[S, Id] => TraversalState[S, Id])(completionState: S): TraversalState[S, Id]
}

object TraversalState {
  def stack[S, Id]: TraversalState[S, Id] =
    StackBasedState[S, Id](List.empty)

  def queue[S, Id]: TraversalState[S, Id] =
    QueueBasedState[S, Id](Queue.empty)
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

  override def triggerCompletion(f: TraversalState[S, Id] => TraversalState[S, Id])(completionState: S): TraversalState[S, Id] =
    f(add(completionState))
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

  override def triggerCompletion(f: TraversalState[S, Id] => TraversalState[S, Id])(completionState: S): TraversalState[S, Id] =
    f(this).add(completionState)
}