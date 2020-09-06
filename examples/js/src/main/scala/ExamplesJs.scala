import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

trait ExampleApp {
  @JSExport
  def main(args: Array[String]): Unit = println("finished example.")
}

@JSExportTopLevel("dijkstra")
object DijkstraExampleApp extends DijkstraExample with ExampleApp

@JSExportTopLevel("bfs")
object BfsExampleApp extends BfsExample with ExampleApp

@JSExportTopLevel("cats")
object CatsExampleApp extends CatsExample with ExampleApp

@JSExportTopLevel("customGraph")
object CustomGraphExampleApp extends CustomGraphExample with ExampleApp

@JSExportTopLevel("dfs")
object DfsExampleApp extends DfsExample with ExampleApp

@JSExportTopLevel("graphml")
object GraphMLRendererExampleApp extends GraphMLExample with ExampleApp

@JSExportTopLevel("simple")
object SimpleGraphExampleApp extends SimpleGraphExample with ExampleApp

@JSExportTopLevel("topologicalSort")
object TopologicalSortingExampleApp extends TopologicalSortingExample with ExampleApp

@JSExportTopLevel("json")
object JsonExampleApp extends JsonExample with ExampleApp