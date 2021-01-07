package com.flowtick.graphs

import cats.effect.IO
import cats.effect.concurrent.Ref
import com.flowtick.graphs.editor._
import org.apache.logging.log4j.{LogManager, Logger}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.FileOutputStream

trait EditorBaseSpec extends AnyFlatSpec with Matchers with EditorMain { self =>
  val log: Logger = LogManager.getLogger(getClass)

  val lastExported: Ref[IO, Option[ExportedGraph]] = Ref.unsafe(None)

  def withEditor[T](f: EditorInstance => T): T =
    createEditor(_ => List(
      printView
    ))(EditorConfiguration()).flatMap(editor => IO(f(editor))).unsafeRunSync()

  private def printView: EditorComponent = new EditorComponent {
    override def init(model: EditorModel): IO[Unit] = IO.unit

    override def eval: Eval = ctx => ctx.effect(this) {
      case Reset => IO {
        ctx.model.graph.edges.foreach(log.debug)
      }

      case export@ExportedGraph(name, value, format) => lastExported.update(_ => Some(export)) *> IO {
        val out = new FileOutputStream(s"target/${self.getClass.getName}_last_exported_$name" + format.`extension`)
        out.write(value.getBytes("UTF-8"))
        out.flush()
        out.close()
      }

      case event => IO(log.debug(event))
    }
  }
}
