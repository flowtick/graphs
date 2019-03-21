## GraphML support

`graphs` supports rendering and loading (currently only on the JVM) of graphs to GraphML XML.

You need to add the `graphs-graphml` dependency to use that:

@@@vars
```scala
libraryDependencies += "com.flowtick" %% "graphs-graphml" % "$version$"
```
@@@

@@snip [SimpleGraphApp.scala](../examples/shared/src/main/scala/GraphMLRendererExample.scala)

This format is used by the [yed editor](https://www.yworks.com/products/yed), so graphs can be edited and 
layouted there.

