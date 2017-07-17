package com.flowtick.graphs.traversal

import com.flowtick.graphs.Node

trait Traversal[N <: Node] {
  def run: List[N]

  def onVisit(f: N => Any): Traversal[N]
  def onComplete(f: N => Any): Traversal[N]
  def onBacktrack(f: N => Any): Traversal[N]
}
