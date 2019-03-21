import com.flowtick.graphs.layout.JGraphXLayouter

object DijkstraExampleApp extends DijkstraExample with App
object BfsExampleApp extends BfsExample with App
object CatsExampleApp extends CatsExample with App
object CustomGraphExampleApp extends CustomGraphExample with App
object DfsExampleApp extends DfsExample with App
object GraphMLRendererExampleApp extends GraphMLRendererExample with App {
  override val layout = JGraphXLayouter
}
object SimpleGraphExampleApp extends SimpleGraphExample with App
object TopologicalSortingExampleApp extends TopologicalSortingExample with App