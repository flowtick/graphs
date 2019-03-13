# Creating graphs

A _graph_ consists of _nodes_ which represent some objects or values and _edges_ which express a
relation between this objects.

`graphs` has a default implementation of a graph that you can use with arbitrary node types.
Using this default implementation, you can instantly start creating simple graphs:

@@snip [SimpleGraphApp.scala](../examples/src/main/scala/SimpleGraphApp.scala)

Edges can also have values associated with them, for example a 
distance between nodes that represent locations. 

Numeric edge values can be used in algorithms like Dijkstras 
algorithm to find the shortest path between two nodes.

@@snip [SimpleGraphApp.scala](../examples/src/main/scala/DijkstraApp.scala){ #cities }

## Core Types

In `graphs` the core type is the graph type-class:

@@snip [Graph.scala](../core/shared/src/main/scala/com/flowtick/graphs/Graph.scala){ #graph }

You can see that is a type constructor with a higher-kinded type `G` for a graph type that has 3 _holes_ (type parameters) in it: 
`G[_, _, _]`  and a second higher-kinded type with 2 parameters: `ET[_, _]` which is the edge type.

This allows to parametrize the graphs: 

* by the edge type which defines the relation between nodes
* by the value type of the edges
* by the value type of the node
* by the value type of the graph (to allow carry it some meta information like a description or groups of nodes)

The edge type is parametrized by the edge value type and the node value type.

In the methods of the type-class you see how the type parameters are applied to the type constructors: 
`G[ET[V, N], N, M]` which can be read as 
*a graph with value M and edge type ET of edge values V connecting nodes of type N*.

## Custom Graph Types

Since the graph type itself is parametrized, you can just plug in your types. 
You only need to define how nodes can be identified:

@@snip [CustomGraphApp.scala](../examples/src/main/scala/CustomGraphApp.scala){#custom_graph}

### Default Graph

If you want to change the graph type itself for some reason, you should look at 
the default graph implementation, which shows a complete example on how to implement the type class instances:

@@snip [DefaultGraph.scala](../core/shared/src/main/scala/com/flowtick/graphs/defaults/package.scala)
