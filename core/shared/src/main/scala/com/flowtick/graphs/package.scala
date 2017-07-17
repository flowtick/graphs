package com.flowtick

package object graphs {
  def n(id: String) = DefaultNode(id)

  implicit class NodeOps[N <: Node](node: N) {
    def ~>[E <: Edge[N]](target: N) = DefaultDirectedEdge(None, node, target)
    def ~>[E <: Edge[N]](target: N, label: String) = DefaultDirectedEdge(Some(label), node, target)

    def ~[E <: Edge[N]](target: N): UndirectedEdge[N] = DefaultUndirectedEdge(None, node, target)
    def ~[T, E <: WeightedEdge[T, N]](value: T, target: N)(implicit ordering: Numeric[T]): WeightedEdge[T, N] = DefaultWeightedEdge(value, None, node, target)
    def ~[T, E <: WeightedEdge[T, N]](value: T, target: N, label: String)(implicit ordering: Numeric[T]): WeightedEdge[T, N] = DefaultWeightedEdge(value, Some(label), node, target)
  }
}
