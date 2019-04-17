# Setup

Add the dependency to your build:

@@@vars
```scala
libraryDependencies += "com.flowtick" %% "graphs-core" % "$version$"
```
@@@

Using the [ammonite REPL](http://ammonite.io) you can quickly play around with (directed) Graphs:

@@@vars
```scala
import $ivy.`com.flowtick:graphs-core_2.12:$version$`, com.flowtick.graphs.defaults._, com.flowtick.graphs.defaults.directed._ 

Graph.from(Seq(n("1") --> n("2")))
```
@@@