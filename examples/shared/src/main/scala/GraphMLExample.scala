import com.flowtick.graphs.Graph
import com.flowtick.graphs.defaults._
import com.flowtick.graphs.graphml._

trait GraphMLExample {
  val graphMLCities: Graph[GraphMLEdge[Int], GraphMLNode[String], GraphMLGraph[Unit]] = DijkstraGraph.cities.toGraphML()

  println(fromGraphML[Int, String, Unit](graphMLCities.xml.toString()))
  println(graphMLCities)
  // Graph(GraphMLGraph((),Some(G),List()),Map(GraphMLNode(Erfurt,Erfurt, ...
}
