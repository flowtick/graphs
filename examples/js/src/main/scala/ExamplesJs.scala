import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("dijkstra")
object DijkstraExampleApp extends DijkstraExample with App

@JSExportTopLevel("bfs")
object BfsExampleApp extends BfsExample with App

@JSExportTopLevel("cats")
object CatsExampleApp extends CatsExample with App

@JSExportTopLevel("customGraph")
object CustomGraphExampleApp extends CustomGraphExample with App

@JSExportTopLevel("dfs")
object DfsExampleApp extends DfsExample with App

@JSExportTopLevel("graphml")
object GraphMLRendererExampleApp extends GraphMLExample with App

@JSExportTopLevel("simple")
object SimpleGraphExampleApp extends SimpleGraphExample with App

@JSExportTopLevel("topologicalSort")
object TopologicalSortingExampleApp extends TopologicalSortingExample with App