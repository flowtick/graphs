import com.flowtick.graphs.defaults._
import com.flowtick.graphs.defaults.directed._
import com.flowtick.graphs.graphml.{ GraphMLImporter, GraphMLRenderer }
import com.flowtick.graphs.layout.{ GraphLayout, ShapeDefinition }
import com.flowtick.graphs.{ Edge, Graph, Identifiable, Labeled }

trait GraphMLRendererExample {
  def layout: GraphLayout

  def toXml[G[_, _, _], V, N, M](g: G[V, N, M])(implicit graph: Graph[G], identifiable: Identifiable[N], edgeLabel: Labeled[Edge[V, N], String]): String = {
    val graphXml = new GraphMLRenderer().render[G, V, N, M](
      g,
      layout,
      (_: N) => Some(ShapeDefinition(rounded = true, color = "#AAAAAA")))

    graphXml.toString()
  }

  val writtenXml = toXml(DijkstraGraph.cities)
  println(GraphMLImporter.fromXml[DefaultGraph, Unit, Unit](writtenXml))
  // Right(DefaultGraph(GraphMLGraph(Some(G)),ListBuffer(GraphMLNode(Frankfurt,Some(Frankfurt),Map(graphics -> ...
}
