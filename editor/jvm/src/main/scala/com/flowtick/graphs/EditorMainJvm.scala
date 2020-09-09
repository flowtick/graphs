package com.flowtick.graphs
import java.io.FileInputStream

import cats.effect.{ExitCode, IO, IOApp}

object EditorMainJvm extends IOApp with EditorMain {
  def printView(bus: EditorMessageBus): EditorComponent = new EditorComponent {
    override def init(model: EditorModel): IO[Unit] = IO.unit

    override def eval: Eval = ctx => ctx.effect(this) {
      case SetGraph(graphml) => IO {
        graphml.graph.edges.foreach(println)
      }
      case event => IO(println(event))
    }
  }

  override def run(args: List[String]): IO[ExitCode] = for {
    editor <- createEditor(bus => List((printView(bus))))(None, None, None)
    xmlContent <- IO {
      scala.io.Source
        .fromInputStream(new FileInputStream("/home/adrobisch/test.graphml"))
        .getLines()
        .mkString("\n")
    }
    (bus, _) = editor
    _ <- bus.publish(Load(xmlContent, GraphMLFormat))
  } yield ExitCode.Success
}
