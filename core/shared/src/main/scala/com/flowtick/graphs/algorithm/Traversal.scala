package com.flowtick.graphs.algorithm

import scala.collection.mutable.ListBuffer

trait Traversal[N] { self =>
  def run: Seq[N]

  protected val visitCallbacks: ListBuffer[N => Any] = ListBuffer.empty
  protected val completeCallbacks: ListBuffer[N => Any] = ListBuffer.empty
  protected val backtrackCallbacks: ListBuffer[N => Any] = ListBuffer.empty

  def onVisit(f: N => Any): self.type = {
    visitCallbacks += f
    self
  }

  def onComplete(f: N => Any): self.type = {
    completeCallbacks += f
    this
  }

  def onBacktrack(f: N => Any): self.type = {
    backtrackCallbacks += f
    this
  }
}
