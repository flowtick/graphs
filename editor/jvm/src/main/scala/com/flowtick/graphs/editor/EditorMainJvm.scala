package com.flowtick.graphs.editor

import cats.effect.IO
import com.flowtick.graphs._
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

  val initEditor: IO[Unit] = for {
    editor <- createEditor(bus => List(
      new RoutingFeature,
      new EditorModelUpdate,
      new EditorViewJavaFx(bus, wholeLayout),
      new EditorMenuJavaFx(bus, wholeLayout, stage)
    ))(None, None, None)
    (bus, _) = editor
    _ <- bus.publish(SetGraph(GraphML.empty))
  } yield ()

  initEditor.unsafeRunSync()
}