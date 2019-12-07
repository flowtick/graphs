import com.flowtick.graphs.Graph

trait GraphMLExample {
  {
    import com.flowtick.graphs.defaults._
    import com.flowtick.graphs.graphml._
    val graphMLCities: Graph[GraphMLEdge[Int], GraphMLNode[String], GraphMLGraph[Unit]] = DijkstraGraph.cities.toGraphML()

    println(fromGraphML[Int, String, Unit](graphMLCities.xml.toString()))
    println(graphMLCities)
    // ImmutableGraph(GraphMLGraph((),Some(G),List()),Map(GraphMLNode(Erfurt,Erfurt, ...
  }

  {
    import com.flowtick.graphs.defaults._
    import com.flowtick.graphs.graphml._
    val graph = Graph.fromEdges[Unit, String](Set(n("A") --> n("B")))
    val xml = graph.toGraphML().xml
    println(fromGraphML[Unit, String, Unit](xml.toString))
  }
}
