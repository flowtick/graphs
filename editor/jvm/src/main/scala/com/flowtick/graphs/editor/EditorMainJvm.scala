package com.flowtick.graphs.editor

import java.io.FileInputStream

import cats.effect.IO
import com.flowtick.graphs._
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.control.{Menu, MenuBar, MenuItem}
import scalafx.scene.layout.BorderPane
import scalafx.stage.FileChooser

import scala.util.Try

object EditorMainJvm extends JFXApp with EditorMain {
  lazy val openItem: MenuItem = new MenuItem("Open") {
    onAction = event => {
      event.consume()

      Try {
        val fileChooser = new FileChooser()
        fileChooser.setTitle("Open Resource File")
        Option(fileChooser.showOpenDialog(stage))
      }.map {
        case Some(file) => (IO(editorPane.clear()) *> loadGraph(file.getAbsolutePath)).unsafeRunSync()
        case _ =>
      }
    }
  }

  lazy val menu = {
    val bar = new MenuBar {
      menus = List(
        new Menu("File") {
          items = List(
            openItem
          )
        }
      )
    }
    bar.setViewOrder(-1.0) // lowest comes first
    bar
  }

  lazy val editorPane = new EditorGraphPane

  lazy val wholeLayout = new BorderPane {
    top = menu
    center = editorPane
    style = s"""-fx-background: #DDD;""".stripMargin
  }

  lazy val editorScene = new Scene(wholeLayout) {
  }

  stage = new JFXApp.PrimaryStage {
    title.value = "graphs editor"
    width = 600
    height = 450
    scene = editorScene
  }

  def fxView(bus: EditorMessageBus): EditorComponent = new EditorViewJavaFx(editorPane)

  def loadGraph(path: String): IO[Unit] = for {
    editor <- createEditor(bus => List(fxView(bus), new EditorModelUpdate))(None, None, None)
    xmlContent <- IO {
      scala.io.Source
        .fromInputStream(new FileInputStream(path))
        .getLines()
        .mkString("\n")
    }
    (bus, _) = editor
    _ <- bus.publish(Load(xmlContent, GraphMLFormat))
  } yield ()
}