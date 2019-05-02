import com.flowtick.graphs.graphml._

trait GraphMLExample {
  val graphXml: scala.xml.NodeSeq = DijkstraGraph.cities.toGraphML.xml

  println(fromGraphML[Unit, Unit, Unit](graphXml.toString()))
  // Right(DefaultGraph(GraphMLGraph(Some(G)),ListBuffer(GraphMLNode(Frankfurt,Some(Frankfurt),Map(graphics -> ...
}
