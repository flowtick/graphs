# Creating graphs

A _graph_ consists of _nodes_ which represent some objects or values and _edges_ which express a
relation between this objects.

`graphs` has a default builder `-->` that you can use with arbitrary node types.
By importing this builder, you can instantly start creating simple graphs:

@@snip [SimpleGraphApp.scala](../../../../examples/shared/src/main/scala/examples/SimpleGraphExample.scala){ #simple_graph }

Edges can also have values associated with them, for example a 
distance between nodes that represent locations. 

Numeric edge values can be used in algorithms like Dijkstras 
algorithm to find the shortest path between two nodes.

@@snip [DijkstraExample.scala](../../../../examples/shared/src/main/scala/examples/DijkstraExample.scala){ #cities }

## Core

In `graphs` the core type is the `Graph` type.

It is parametrized over three types:

* the value type of the edges (named `E`) 
* the value type of the node (named `N`)

The meta value allows carrying additional information on the graph itself like a description or groups / layers of nodes.
In `graphs` transformations are defined on nodes, which is why the node type is on the right side of the type parameter 
list.

A value of a graph instance of type `Graph[Double, String]` would be described as
> a graph with edges of type `Double` value connecting nodes of type `String`

`Graph` has common methods to work with graph instances:

@@snip [Graph.scala](../../../../core/shared/src/main/scala/com/flowtick/graphs/Graph.scala){ #graph }

## Identity

Nodes and Edges have an `id` field of type `String`. Most of the graph API is built around this `id`.
During graph creation, the `id` is derived from the node / edge value via the `Identifiable` type:

@@snip [Graph.scala](../../../../core/shared/src/main/scala/com/flowtick/graphs/Graph.scala){ #identifiable } 

The defaults contain some default identities for common primitive types, for complex custom types you 
need to provide a corresponding instance.

## Custom Graph Types

Since the graph type itself is parametrized, you can just plug in your types:

@@snip [CustomGraphApp.scala](../../../../examples/shared/src/main/scala/examples/CustomGraphExample.scala){#custom_graph}


