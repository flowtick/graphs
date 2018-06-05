import java.io.FileOutputStream

import com.flowtick.graphs.defaults._
import com.flowtick.graphs.graphml.GraphMLRenderer
import com.flowtick.graphs.layout.{ JGraphXLayouter, ShapeDefinition }

object GraphMLRendererApp extends App {
  val graphXml = new GraphMLRenderer().render(
    DijkstraGraph.cities,
    JGraphXLayouter,
    (_: DefaultNode) => Some(ShapeDefinition(rounded = true, color = "#AA0000")))

  val output = new FileOutputStream("target/cities.graphml")
  output.write(graphXml.toString().getBytes)
  output.flush()
  output.close()
}
