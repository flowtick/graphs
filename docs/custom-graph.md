# Custom Graph Types

Creating a custom graph type is just a matter of extending the core interfaces `Node`, `Graph`, `Edge` with
custom implementations.

## Default Graph Example

The default graph implementation shows how to define a simple node type with one `id`-property and bunch of builders for
the different edge types:

@@snip [DefaultGraph.scala](../core/shared/src/main/scala/com/flowtick/graphs/defaults/DefaultGraph.scala)

## Arrow-style builder methods

To add some sugar for the graph creation, have a look at tne `NodeOps`-types which allow to define arrow like
builder methods on the node types:

@@snip [DefaultGraph.scala](../core/shared/src/main/scala/com/flowtick/graphs/defaults/package.scala)