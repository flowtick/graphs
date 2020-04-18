## GraphML support

`graphs` supports exporting and loading of graphs to GraphML XML. 
This format is used by the [yed editor](https://www.yworks.com/products/yed), so graphs can be edited and 
layouted there.

You need to add the `graphs-graphml` dependency to use it:

@@@vars
```scala
libraryDependencies += "com.flowtick" %% "graphs-graphml" % "$version$"
```
@@@

### Conversion to GraphML

This creates a default graph and converts it to a GraphML graph.

@@snip [GraphMLExample.scala](../examples/shared/src/main/scala/GraphMLExample.scala){ #simple-graphml }

### Custom Node Types

Its possible to create GraphML graphs directly using the `ml` edge builder and serialize your own node types, 
this is implemeted using shapeless:

@@snip [GraphMLExample.scala](../examples/shared/src/main/scala/GraphMLExample.scala){ #custom-node-graphml }
