package com.flowtick.graphs.traversal

import com.flowtick.graphs.Node

import scala.collection.mutable

trait TraversalBuffer[N <: Node] {
  def addCurrent(node: N)
  def get: N
  def nonEmpty: Boolean
}

class FifoBuffer[N <: Node](queue: mutable.Queue[N] = new mutable.Queue[N]()) extends TraversalBuffer[N] {
  override def addCurrent(node: N): Unit = queue.enqueue(node)

  override def get: N = queue.dequeue()

  override def nonEmpty: Boolean = queue.nonEmpty
}

class LifoBuffer[N <: Node](stack: mutable.Stack[N] = new mutable.Stack[N]()) extends TraversalBuffer[N] {
  override def addCurrent(node: N): Unit = stack.push(node)

  override def get: N = stack.pop()

  override def nonEmpty: Boolean = stack.nonEmpty
}