package com.flowtick.graphs

import cats.effect.IO
import cats.effect.kernel.Ref
import cats.effect.unsafe.implicits.global
import com.flowtick.graphs.editor._
import com.flowtick.graphs.editor.view.SVGRendererJvm
import org.apache.logging.log4j.{LogManager, Logger}
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.FileOutputStream

trait EditorBaseSpec
    extends AnyFlatSpec
    with Matchers
    with EditorMain
    with ScalaFutures
    with IntegrationPatience { self =>
  val log: Logger = LogManager.getLogger(getClass)

  val lastExported: Ref[IO, Option[ExportedGraph]] = Ref.unsafe(None)

  def shouldRenderImage: Boolean = false

  def withEditor[T](f: EditorInstance => T): T =
    createEditor(bus =>
      List(
        testView(bus)
      )
    )(EditorConfiguration()).flatMap(editor => IO(f(editor))).unsafeToFuture().futureValue

  protected def testView(bus: EditorMessageBus): EditorComponent =
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

            lastExported.update(_ => Some(export)) *> IO {
              val out = new FileOutputStream(fileName + format.`extension`)
              out.write(value.getBytes("UTF-8"))
              out.flush()
              out.close()
            } *> {
              if (shouldRenderImage) {
                val renderer = SVGRendererJvm()
                renderer
                  .renderGraph(
                    ctx.model.graph,
                    ctx.model.layout,
                    ctx.model.styleSheet
                  )
                  .flatMap(_ => IO.fromTry(renderer.toXmlString))
                  .flatMap { xmlString =>
                    IO {
                      val out = new FileOutputStream(fileName + ".svg")
                      out.write(xmlString.getBytes("UTF-8"))
                      out.flush()
                      out.close()
                    }
                  }
              } else IO.unit
            }

          case event => IO(log.debug(event))
        }
    }
}
