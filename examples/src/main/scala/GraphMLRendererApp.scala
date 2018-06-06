import java.io.FileOutputStream

import com.flowtick.graphs.{ Graph, Identifiable, Labeled }
import com.flowtick.graphs.defaults._
import com.flowtick.graphs.graphml.GraphMLRenderer
import com.flowtick.graphs.layout.{ JGraphXLayouter, ShapeDefinition }

object GraphMLRendererApp extends App {
  def writeGraphXML[N, E](g: Graph[N, E], fileName: String)(implicit identifiable: Identifiable[N], edgeLabel: Labeled[E, String]): Unit = {
    val graphXml = new GraphMLRenderer().render(
      g,
      JGraphXLayouter,
      (_: N) => Some(ShapeDefinition(rounded = true, color = "#AAAAAA")))

    val output = new FileOutputStream(fileName)
    output.write(graphXml.toString().getBytes)
    output.flush()
    output.close()
  }

  writeGraphXML(DijkstraGraph.cities, "target/cities.graphml")
  // writeGraphXML(TopologicalSortingApp.clothingDependencies, "target/clothing.graphml")
}
