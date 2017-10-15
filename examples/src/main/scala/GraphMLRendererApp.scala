import java.io.FileOutputStream

import com.flowtick.graphs.Node
import com.flowtick.graphs.graphml.GraphMLRenderer
import com.flowtick.graphs.rendering.ShapeDefinition

object GraphMLRendererApp extends App {
  val graphXml = new GraphMLRenderer().render(DijkstraGraph.cities, (node: Node) => Some(
    ShapeDefinition(rounded = true, color = "#AA0000")))

  val output = new FileOutputStream("target/cities.graphml")
  output.write(graphXml.toString().getBytes)
  output.flush()
  output.close()
}
