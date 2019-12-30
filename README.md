[![travis ci](https://api.travis-ci.org/flowtick/graphs.svg?branch=master)](https://travis-ci.org/flowtick/graphs)
[![codecov](https://codecov.io/gh/flowtick/graphs/branch/master/graph/badge.svg)](https://codecov.io/gh/flowtick/graphs)
[![latest release](https://img.shields.io/maven-central/v/com.flowtick/graphs-core_2.12.svg?label=Maven%20Central)](https://search.maven.org/search?q=g:com.flowtick%20AND%20a:graphs*)

# graphs

`graphs` is a simple [graph](https://en.wikipedia.org/wiki/Graph_theory) library for Scala

# Example

@@snip [SimpleGraphApp.scala](examples/shared/src/main/scala/SimpleGraphExample.scala)

# Documentation

Please check the [guide](https://flowtick.github.io/graphs) and the
[API docs](https://flowtick.github.io/graphs/latest/api/com/flowtick/graphs)

# Features
  * Simple graph creation API
  * Depth-first traversal / search
  * Breadth-first traversal / search
  * Topological sorting
  * Dijkstras algorithm for shortest paths
  * GraphML import / export (experimental)
  * <strike>force based layout</strike> (planned)
  * cross compiled for Scala 2.12, 2.13, Scala.js

# Motivation

`graphs` was created to explore different type encoding for graphs and implement well-known algorithms

## Goals
  * Support [Scala.js](https://www.scala-js.org)
  * Support [GraphML](https://de.wikipedia.org/wiki/GraphML)
  * Usages of the library and the core interfaces should be intuitive
  * The codebase should follow current idioms and integrate with mainstream libraries for Scala

## Non-Goals
  * Support all possible graph types / scenarios
  * Provide a purely functional library

## Alternatives

[Graph for Scala](http://scala-graph.org) is probably the most established graph library for Scala and supports many kinds of graphs 
explicitly (custom syntax etc.) with a big variety of algorithms and extensions (json, dot support). 

Its still being worked on and recently added support for Scala.js. It might have a steeper learning curve but is more
battle-tested and powerful then `graphs`.

[quiver](https://github.com/Verizon/quiver) follows [Martin Erwigs Functions Graph Library](http://web.engr.oregonstate.edu/~erwig/fgl/haskell)
and appears to be more or less abandoned. Its less focused on algorithms but provides a more functional perspective 
on graphs.

`graphs` is inspired and influenced by both libraries. Please check them out to see if they fit your use case 
and preferences better.
 
# License

graphs is published under the terms of the Apache 2.0 License. See the [LICENSE](LICENSE) file.