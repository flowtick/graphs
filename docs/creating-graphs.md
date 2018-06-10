# Creating graphs

A _graph_ consists of _nodes_ which represent some objects or values and _edges_ which express a
relation between this objects and values. In `graphs` the base type for a graph is therefore
a trait with the type parameters for node and the edge type:

@@snip [Graph.scala](../core/shared/src/main/scala/com/flowtick/graphs/Graph.scala){ #graph }

There is default implementation of a graph which has just one property for its nodes, which is an `id`
to identify the node. Using this default implementation, you can instantly start creating graph instances:

@@snip [SimpleGraphApp.scala](../examples/src/main/scala/SimpleGraphApp.scala)

## Custom Graph Types

Creating a custom graph type is just a matter of extending the core trait `Edge` with
custom implementations and an edge builder to create instances:

@@snip [CustomGraphApp.scala](../examples/src/main/scala/CustomGraphApp.scala){#custom_graph}

### Default Graph

The default graph implementation shows another example on how to define a simple node type and a bunch of builders for
the different edge types:

@@snip [DefaultGraph.scala](../core/shared/src/main/scala/com/flowtick/graphs/defaults/package.scala)
