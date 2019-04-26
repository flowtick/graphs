import com.flowtick.graphs.defaults._
import com.flowtick.graphs.graphml.{ GraphMLImporter, GraphMLDatatype }
import com.flowtick.graphs.layout.{ GraphLayout, ShapeDefinition }

trait GraphMLRendererExample {
  def layout: GraphLayout

  val graphXml: scala.xml.Elem = new GraphMLDatatype().render(
    DijkstraGraph.cities,
    layout)

  println(GraphMLImporter.fromXml[Unit, Unit](graphXml.toString()))
  // Right(DefaultGraph(GraphMLGraph(Some(G)),ListBuffer(GraphMLNode(Frankfurt,Some(Frankfurt),Map(graphics -> ...
}
