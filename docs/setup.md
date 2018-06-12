# Setup

Add the dependency to your build:

@@@vars
```scala
libraryDependencies += "com.flowtick" %% "graphs-core" % "$version$"
```
@@@

Using the [ammonite REPL](http://ammonite.io) you can quickly play around with (directed) Graphs:

```scala
import $ivy.`com.flowtick:graphs-core_2.12:$version$`, com.flowtick.graphs.defaults._, com.flowtick.graphs.defaults.directed._ 

DefaultGraph.create(Seq(n("1") -> n("2")))
```