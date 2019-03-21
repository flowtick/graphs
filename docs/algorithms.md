# Algorithms

graphs allows ad-hoc extension of graph instances so that you can execute some common operations and algorithms on it.

Currently the following algorithms are supported:

## Depth-first traversal

A depth first traversal visits every node _per branch / path_ which means that for every node the first child will
be visited and the search will be continued there, before going to its siblings.

Every node will be visited.

@@snip [DfsExample.scala](../examples/shared/src/main/scala/DfsExample.scala)

## Breadth-first traversal

A breadth first traversal visits every node _per layer_, which means that first all child nodes
will be visited before continuing with their children.

Every node will be visited.

@@snip [BfsExample.scala](../examples/shared/src/main/scala/BfsExample.scala)

## Topological sorting using a depth-first approach

<blockquote>
https://en.wikipedia.org/wiki/Topological_sorting:

"topological ordering of a directed graph is a linear ordering of its vertices such that for every
directed edge uv from vertex u to vertex v, u comes before v in the ordering.
For instance, the vertices of the graph may represent tasks to be performed, and the edges may represent
constraints that one task must be performed before another"
</blockquote>


@@snip [TopologicalSortingExample.scala](../examples/shared/src/main/scala/TopologicalSortingExample.scala)

## Dijkstras algorithm for shortest paths

<blockquote>
https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm:

For a given source node in the graph, the algorithm finds the shortest path between that node and every other.
It can also be used for finding the shortest paths from a single node to a single destination node by stopping
the algorithm once the shortest path to the destination node has been determined.
</blockquote>

@@snip [DijkstraExample.scala](../examples/shared/src/main/scala/DijkstraExample.scala)