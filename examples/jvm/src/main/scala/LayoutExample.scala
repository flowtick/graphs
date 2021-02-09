import com.flowtick.graphs.editor.view.{EditorRendererJvm, SVGRendererOptions, SVGTranscoder}
import com.flowtick.graphs.layout.{ELkLayout, GraphLayoutConfiguration, LayoutDirection, LayoutType}
import com.flowtick.graphs.style._

import java.io.FileOutputStream

object LayoutExample extends App {
  import com.flowtick.graphs._
  import com.flowtick.graphs.defaults._
  import com.flowtick.graphs.defaults.label._
  import com.flowtick.graphs.style.defaults._

  val graph: Graph[Int, String] = DijkstraGraph.cities

  val layout = ELkLayout.layout(graph, GraphLayoutConfiguration(nodeWidth = 50, nodeHeight = 50, direction = Some(LayoutDirection.Down), layoutType = Some(LayoutType.Layered)))
  val renderer = EditorRendererJvm(options = SVGRendererOptions(padding = Some(100)))
  val nodeShape = NodeShape(
    fill = Some(Fill(color = Some("#aaa"))),
    shapeType = Some(ShapeType.RoundRectangle),
    image = Some("city"),
    labelStyle = Some(NodeLabel(textColor = Some("#ccc"), fontSize = Some("16"), border = Some(BorderStyle(color = "#222", width = Some(0.6)))))
  )

  val edgeShape = EdgeShape(
    arrows = Some(Arrows(source = Some("circle"), target = Some("standard")))
  )

  val styleSheet = StyleSheet()
    .withNodeDefault(nodeShape)
    .withEdgeDefault(edgeShape)
    .withImage("city", ImageSpec("https://openmoji.org/data/color/svg/1F3D9.svg", imageType = ImageType.url))

  renderer
    .translateAndScaleView(0,0, 2.0)
    .renderGraph(graph, layout, styleSheet).unsafeRunSync()

  val out = new FileOutputStream("target/layout_example.svg")
  out.write(renderer.toXmlString.get.getBytes("UTF-8"))
  out.flush()
  out.close()

  val pngOut = new FileOutputStream("target/layout_example.png")
  val pngBytes = SVGTranscoder.svgXmlToPng(renderer.toXmlString.get, None, None).unsafeRunSync()
  pngOut.write(pngBytes)
  pngOut.flush()
  pngOut.close()
}
