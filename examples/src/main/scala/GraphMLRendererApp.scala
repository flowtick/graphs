import java.io.FileOutputStream

import com.flowtick.graphs.graphml.GraphMLRenderer

object GraphMLRendererApp extends App {
  val graphXml = new GraphMLRenderer().render(DijkstraGraph.cities)

  val output = new FileOutputStream("target/cities.graphml")
  output.write(graphXml.toString().getBytes)
  output.flush()
  output.close()
}
