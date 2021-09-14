# Setup

Add the dependency to your build:

@@@vars
```scala
libraryDependencies += "com.flowtick" %% "graphs-core" % "$version$"
```
@@@

Using the [ammonite REPL](http://ammonite.io) you can quickly play around with graphs:

@@@vars
```scala
import $ivy.`com.flowtick:graphs-core_2.13:$version$`
```
@@@

@@snip [SimpleGraphApp.scala](../../../../examples/shared/src/main/scala/examples/SimpleGraphExample.scala){ #simple_graph }
