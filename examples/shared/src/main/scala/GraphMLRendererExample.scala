import com.flowtick.graphs.defaults._
import com.flowtick.graphs.graphml.{ GraphMLImporter, GraphMLSerializer }
import com.flowtick.graphs.layout.{ GraphLayout, ShapeDefinition }

trait GraphMLRendererExample {
  def layout: GraphLayout

  val graphXml: scala.xml.Elem = new GraphMLSerializer().render(
    DijkstraGraph.cities,
    layout,
    (_: String) => Some(ShapeDefinition(rounded = true, color = "#AAAAAA")))

  println(GraphMLImporter.fromXml[Unit, Unit](graphXml.toString()))
  // Right(DefaultGraph(GraphMLGraph(Some(G)),ListBuffer(GraphMLNode(Frankfurt,Some(Frankfurt),Map(graphics -> ...
}
