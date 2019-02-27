import java.io.FileOutputStream

import com.flowtick.graphs.{ EdgeType, Graph, Identifiable, Labeled }
import com.flowtick.graphs.defaults._
import com.flowtick.graphs.defaults.directed._
import com.flowtick.graphs.graphml.{ GraphMLImporter, GraphMLRenderer }
import com.flowtick.graphs.layout.{ JGraphXLayouter, ShapeDefinition }

object GraphMLRendererApp extends App {
  def writeGraphXML[G[_, _, _], E[_, _], V, N, M](g: G[E[V, N], N, M], fileName: String)(implicit graph: Graph[G, E], edgeType: EdgeType[E], identifiable: Identifiable[N], edgeLabel: Labeled[E[V, N], String]): String = {
    val graphXml = new GraphMLRenderer().render[G, E, V, N, M](
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
  println(new GraphMLImporter[DefaultGraph, Edge]().fromXml(writtenXml))
  // Right(DefaultGraph(GraphMLGraph(Some(G)),ListBuffer(GraphMLNode(Frankfurt,Some(Frankfurt),Map(graphics -> ...
}
