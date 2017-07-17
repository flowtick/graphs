Example
=======

A simple dependency graph and its topological order:

```scala

import com.flowtick.graphs._ 
import com.flowtick.graphs.defaults._
import com.flowtick.graphs.algorithm._ 

object GraphApp extends App {
  val graph = Graph.create[DefaultNode, Edge[DefaultNode]] { implicit graph =>
    n("A") ~> n("B")
    n("B") ~> n("C")
    n("D") ~> n("A")
  }

  println(graph.topologicalSort)
  // List(DefaultNode(D), DefaultNode(A), DefaultNode(B), DefaultNode(C))
}
```

Check the [tests](https://bitbucket.org/flowtick/graphs/src/master/core/jvm/src/test/scala/com/flowtick/graphs/) for more examples.