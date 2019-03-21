import com.flowtick.graphs.defaults._
import com.flowtick.graphs.defaults.directed._
import com.flowtick.graphs.graphml.{ GraphMLImporter, GraphMLRenderer }
import com.flowtick.graphs.layout.{ GraphLayout, ShapeDefinition }
import com.flowtick.graphs.{ EdgeType, Graph, Identifiable, Labeled }

trait GraphMLRendererExample {
  def layout: GraphLayout

  def toXml[G[_, _, _], E[_, _], V, N, M](g: G[E[V, N], N, M])(implicit graph: Graph[G, E], edgeType: EdgeType[E], identifiable: Identifiable[N], edgeLabel: Labeled[E[V, N], String]): String = {
    val graphXml = new GraphMLRenderer().render[G, E, V, N, M](
      g,
      layout,
      (_: N) => Some(ShapeDefinition(rounded = true, color = "#AAAAAA")))

    graphXml.toString()
  }

  val writtenXml = toXml(DijkstraGraph.cities)
  println(new GraphMLImporter[DefaultGraph, Edge]().fromXml(writtenXml))
  // Right(DefaultGraph(GraphMLGraph(Some(G)),ListBuffer(GraphMLNode(Frankfurt,Some(Frankfurt),Map(graphics -> ...
}
