import java.io.FileOutputStream

import com.flowtick.graphs.{ Graph, Identifiable, Labeled }
import com.flowtick.graphs.defaults._
import com.flowtick.graphs.graphml.{ GraphMLRenderer, GraphMLImporter }
import com.flowtick.graphs.layout.{ JGraphXLayouter, ShapeDefinition }

object GraphMLRendererApp extends App {
  def writeGraphXML[N, E](g: Graph[N, E], fileName: String)(implicit identifiable: Identifiable[N], edgeLabel: Labeled[E, String]): String = {
    val graphXml = new GraphMLRenderer().render(
      g,
      JGraphXLayouter,
      (_: N) => Some(ShapeDefinition(rounded = true, color = "#AAAAAA")))

    val output = new FileOutputStream(fileName)
    output.write(graphXml.toString().getBytes)
    output.flush()
    output.close()

    graphXml.toString()
  }

  val writtenXml = writeGraphXML(DijkstraGraph.cities, "target/cities.graphml")
  println(new GraphMLImporter().fromXml(writtenXml))
  // Right(GraphMLGraph(Some(G),SomeGraph(Set(GraphMLNode(Karlsruhe,Some(Karlsruhe),Map(graphics -> ...
}
