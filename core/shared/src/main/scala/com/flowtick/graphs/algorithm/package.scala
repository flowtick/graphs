package com.flowtick.graphs

import com.flowtick.graphs.algorithm.Traversal.Step

package object algorithm {
  final case class Path[E, N](steps: List[Step[E, N]])

  implicit class GraphOps[M, E, N](graph: Graph[E, N]) {
    def bfs(startNode: String): StepTraversal[E, N] =
      new BreadthFirstTraversal[E, N](Seq(startNode), graph)
    def dfs(startNode: String): StepTraversal[E, N] =
      new DepthFirstTraversal[E, N](Seq(startNode), graph)
    def topologicalSort: List[Step[E, N]] =
      new TopologicalSort[E, N](graph).sort
    def dijkstra(implicit
        numeric: Numeric[E],
        label: Labeled[Edge[E], E]
    ): DijkstraShortestPath[M, E, N] = new DijkstraShortestPath[M, E, N](graph)

    def paths(startNode: String): List[Path[E, N]] = {
      new DepthFirstTraversal[E, N](Seq(startNode), graph).run
        .foldLeft(List.empty[Path[E, N]]) {
          case (Nil, Visited(step)) =>
            Path(steps = List(step)) :: Nil
          case (path :: tail, Visited(step)) =>
            path.copy(steps = path.steps :+ step) :: tail
          case (path :: tail, Completed(_, _)) =>
            if (path.steps.size == 1) path :: tail // skip empty list
            else Path(path.steps.dropRight(1)) :: path :: tail
          case (paths, _) => paths
        }
    }
  }
}
