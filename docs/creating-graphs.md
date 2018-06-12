# Creating graphs

A _graph_ consists of _nodes_ which represent some objects or values and _edges_ which express a
relation between this objects.

`graphs` has a default implementation of a graph with node values of type `String` which acts as 
the identifier of that node. Using this default implementation, you can instantly start creating simple graph instances:

@@snip [SimpleGraphApp.scala](../examples/src/main/scala/SimpleGraphApp.scala)

The default graph types also support weighted edges, which are edges carrying some kind of value like a 
distance between nodes. Numeric weights can be used in algorithms.

@@snip [SimpleGraphApp.scala](../examples/src/main/scala/DijkstraApp.scala){ #cities }

## Core Types

In `graphs` the base type for a graph is a trait with the type parameters for the node and the edge type:

@@snip [Graph.scala](../core/shared/src/main/scala/com/flowtick/graphs/Graph.scala){ #graph }

## Custom Graph Types

Creating a custom graph type is just a matter of extending the core trait `Edge` with
custom implementations and an edge builder to create instances:

@@snip [CustomGraphApp.scala](../examples/src/main/scala/CustomGraphApp.scala){#custom_graph}

### Default Graph

The default graph implementation shows a more complete example on how to define a simple node type and a bunch of 
builders for different edge types:

@@snip [DefaultGraph.scala](../core/shared/src/main/scala/com/flowtick/graphs/defaults/package.scala)
