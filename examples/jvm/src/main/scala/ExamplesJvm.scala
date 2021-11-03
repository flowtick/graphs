import cats.effect.IO
import com.flowtick.graphs.editor.view.{SVGRendererJvm, SVGRendererOptions, SVGTranscoder}
import com.flowtick.graphs.layout.{ForceDirectedLayout, GraphLayoutOps}
import com.flowtick.graphs.style._
import examples._

import java.io.FileOutputStream

object DijkstraExampleApp extends DijkstraExample with App
object BfsExampleApp extends BfsExample with App
object CatsExampleApp extends CatsExample with App
object CustomGraphExampleApp extends CustomGraphExample with App
object DfsExampleApp extends DfsExample with App
object GraphMLExampleApp extends GraphMLExample with App
object SimpleGraphExampleApp extends SimpleGraphExample with App
object TopologicalSortingExampleApp extends TopologicalSortingExample with App
object JsonExampleApp extends JsonExample with App

object LayoutExampleApp extends LayoutExample with App {
  import com.flowtick.graphs.defaults._
  import com.flowtick.graphs.defaults.label._
  import com.flowtick.graphs.style.defaults._

  implicit val contextShift =
    IO.contextShift(scala.concurrent.ExecutionContext.Implicits.global)
  override def layoutOps: GraphLayoutOps = ForceDirectedLayout

  def writeToFile(path: String, content: Array[Byte]): IO[Unit] = IO {
    val out = new FileOutputStream(path)
    out.write(content)
    out.flush()
    out.close()
  }

  val renderer =
    SVGRendererJvm(options = SVGRendererOptions(padding = Some(100)))
  val nodeShape = NodeShape(
    fill = Some(Fill(color = Some("#aaa"))),
    shapeType = Some(ShapeType.RoundRectangle),
    image = Some("city"),
    labelStyle = Some(
      NodeLabel(
        textColor = Some("#ccc"),
        fontSize = Some("16"),
        border = Some(BorderStyle(color = "#222", width = Some(0.6)))
      )
    )
  )

  val edgeShape = EdgeShape(
    arrows = Some(Arrows(source = Some("circle"), target = Some("standard")))
  )

  val styleSheet = StyleSheet()
    .withNodeDefault(nodeShape)
    .withEdgeDefault(edgeShape)
    .withImage(
      "city",
      ImageSpec(
        "https://openmoji.org/data/color/svg/1F3D9.svg",
        imageType = ImageType.url
      )
    )

  val renderImages = for {
    layoutResult <- IO.fromFuture(IO(layout))
    _ <- renderer
      .translateAndScaleView(0, 0, 2.0)
      .renderGraph(graph, layoutResult, styleSheet)
    _ <- writeToFile(
      "target/layout_example.svg",
      renderer.toXmlString.get.getBytes("UTF-8")
    )
    pngData <- SVGTranscoder.svgXmlToPng(renderer.toXmlString.get, None, None)
    _ <- writeToFile("target/layout_example.png", pngData)
  } yield ()

  renderImages.unsafeToFuture()
}
