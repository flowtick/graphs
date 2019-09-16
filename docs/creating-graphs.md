# Creating graphs

A _graph_ consists of _nodes_ which represent some objects or values and _edges_ which express a
relation between this objects.

`graphs` has a default implementation of a graph that you can use with arbitrary node types.
Using this default implementation, you can instantly start creating simple graphs:

@@snip [SimpleGraphApp.scala](../examples/shared/src/main/scala/SimpleGraphExample.scala)

Edges can also have values associated with them, for example a 
distance between nodes that represent locations. 

Numeric edge values can be used in algorithms like Dijkstras 
algorithm to find the shortest path between two nodes.

@@snip [SimpleGraphApp.scala](../examples/shared/src/main/scala/DijkstraExample.scala){ #cities }

## Core Types

In `graphs` the core type is the `Graph` type-class:

@@snip [Graph.scala](../core/shared/src/main/scala/com/flowtick/graphs/Graph.scala){ #graph }

You can see that is a type constructor with a higher-kinded type `G` for a graph type that has 3 _holes_ (type parameters) in it: 
`G[_, _, _]` .

This allows to parametrize the graphs: 

* by the value type of the edges (usually named `V`) 
* by the value type of the node (usually named `N`)
* by the value type of the graph (usually named `M`) 

The later allows to carry some meta information like a description or groups / layers of nodes.

In the methods of the type class you see how the type parameters are applied to the type constructors: 
`G[V, N, M]` which can be read as 
*a graph with value M and edge values V connecting nodes of type N*.

## Custom Graph Types

Since the graph type itself is parametrized, you can just plug in your types:

@@snip [CustomGraphApp.scala](../examples/shared/src/main/scala/CustomGraphExample.scala){#custom_graph}

### Default Graph

If you want to change the graph type itself for some reason, you should look at 
the default graph implementation, which shows a complete example on how to implement the type class instances:

@@snip [DefaultGraph.scala](../core/shared/src/main/scala/com/flowtick/graphs/defaults/package.scala)
