package com.flowtick.graphs

import cats.effect.IO
import cats.effect.kernel.Ref
import cats.effect.unsafe.implicits.global
import com.flowtick.graphs.editor._
import com.flowtick.graphs.json.schema.Schema
import com.flowtick.graphs.style.StyleSheet
import com.flowtick.graphs.view.SVGRendererJvm
import org.apache.logging.log4j.{LogManager, Logger}
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.FileOutputStream
import scala.collection.mutable.ListBuffer

trait EditorBaseSpec
    extends AnyFlatSpec
    with Matchers
    with EditorMain
    with ScalaFutures
    with IntegrationPatience { self =>
  protected val log: Logger = LogManager.getLogger(getClass)
  protected val events: ListBuffer[EditorEvent] = ListBuffer()
  protected val lastExported: Ref[IO, Option[ExportedGraph]] = Ref.unsafe(None)

  def shouldRenderImage: Boolean = false

  def testComponents(bus: EditorMessageBus): List[EditorComponent] = List.empty

  def testSchema: Schema[EditorSchemaHints] = Schema(
    `type` = Some(Right("object")),
    properties = Some(
      Map(
        "text" -> Schema(
          `type` = Some(Right("string"))
        )
      )
    )
  )

  def testPalette: Palette = Palette(
    stencils = List(
      StencilGroup(
        "test",
        List(Stencil("test", "A Test Stencil", schemaRef = Some("#/$defs/test-schema")))
      )
    ),
    styleSheet = StyleSheet(),
    schema = Schema(
      definitions = Some(
        Map("test-schema" -> testSchema)
      )
    )
  )

  def palettes: Option[List[Palette]] = Some(List(testPalette))

  def editorConfiguration: EditorConfiguration = EditorConfiguration(palettes)

  def withEditor[T](f: EditorInstance => T): T =
    createEditor(bus =>
      List(
        testView
      ) ++ testComponents(bus)
    )(editorConfiguration).flatMap(editor => IO(f(editor))).unsafeToFuture().futureValue

  protected def writeToFile(fileName: String)(content: String): IO[Unit] = IO {
    val out = new FileOutputStream(fileName)
    out.write(content.getBytes("UTF-8"))
    out.flush()
    out.close()
  }

  protected def testView: EditorComponent =
    new EditorComponent {
      override def init(model: EditorModel): IO[Unit] = IO.unit

      override def eval: Eval = ctx =>
        ctx.effect(this) {
          case Reset =>
            IO {
              ctx.model.graph.edges.foreach(log.debug)
            }

          case export @ ExportedGraph(name, value, format) =>
            val fileName =
              s"target/${self.getClass.getName}_last_exported_$name"

            lastExported.update(_ => Some(export)) *>
              writeToFile(fileName + format.extension)(value) *>
              (if (shouldRenderImage) {
                 SVGRendererJvm()
                   .renderGraph(ctx.model)
                   .flatMap(_.toXmlString)
                   .flatMap(writeToFile(fileName + ".svg"))
               } else IO.unit)

          case event =>
            IO {
              events += event
              log.debug(event)
            }
        }
    }
}
