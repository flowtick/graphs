graphs
======

`graphs` is simple graph library for Scala and Scala.js

Features
========

* Simple graph creation API
* Depth-first traversal / search
* Breadth-first traversal / search
* Topological sorting
* Dijkstras algorithm for shortest paths
* GraphML import / export (experimental)
* <strike>force based layout</strike> (planned)
* cross compiled for Scala 2.11, 2.12, Scala.js

Alternatives
============

`graphs` is similar to [Graph for Scala](http://scala-graph.org) but not aiming to model 
every kind of graph, making the core classes smaller and maybe easier to understand. 
If you need more than simple directed / undirected graphs 
please consider using that instead of `graphs`, 
be aware that _Graph for Scala_ does not have Scala.js support currently.

License
=======

graphs is published under the terms of the Apache 2.0 License. See the [LICENSE](LICENSE) file.

@@@ index

* [Setup](setup.md)
* [Creating Graphs](creating-graphs.md)
* [Algorithms](algorithms.md)
* [GraphML support](graphml.md)
* [cats support](cats.md)

@@@
