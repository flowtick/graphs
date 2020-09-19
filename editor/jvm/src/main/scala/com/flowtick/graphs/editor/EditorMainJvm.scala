package com.flowtick.graphs.editor

import java.io.File
import java.net.URL

import cats.effect.IO
import com.flowtick.graphs._
import com.flowtick.graphs.editor.feature.RoutingFeature
import com.flowtick.graphs.graphml.GraphML
import scalafx.application.JFXApp
import scalafx.scene.layout.BorderPane
import scalafx.scene.{Group, Scene}

object EditorMainJvm extends JFXApp with EditorMain {

  lazy val pageRoot = new Group

  lazy val wholeLayout = new BorderPane {
    center = pageRoot
    style = s"""-fx-background: #DDD;""".stripMargin
  }

  lazy val editorScene = new Scene(wholeLayout) {
    val stylePath = sys.env.getOrElse("GRAPHS_THEME", getClass.getClassLoader.getResource("style.css").toURI.toString)
    stylesheets.add(stylePath)
  }

  stage = new JFXApp.PrimaryStage {
    title.value = "graphs editor"
    width = 600
    height = 450
    scene = editorScene
  }

  def initEditor: IO[(EditorMessageBus, List[EditorComponent])] = for {
    editor <- createEditor(bus => List(
      new EditorMenuJavaFx(bus, wholeLayout, stage),
      new EditorViewJavaFx(bus, wholeLayout)
    ))(None, None, None)
    (bus, _) = editor
    _ <- parameters.raw.headOption match {
      case Some(firstArg) =>
        for {
          fileUrl <- IO(new URL(firstArg)).redeemWith(_ => IO(new URL(s"file://${new File(firstArg).getAbsolutePath}")), IO.pure)
          _ <- IO(scala.io.Source.fromURL(fileUrl)).bracket { source =>
            val format = if (fileUrl.getFile.endsWith(".json")) JsonFormat else GraphMLFormat
            bus.publish(Load(source.getLines().mkString("\n"), format))
          }(source => IO(source.close()))
        } yield ()
      case None => bus.publish(SetGraph(GraphML.empty))
    }
  } yield editor

  initEditor.unsafeRunSync()
}