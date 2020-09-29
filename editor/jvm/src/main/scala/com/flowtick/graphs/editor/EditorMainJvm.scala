package com.flowtick.graphs.editor

import java.io.File
import java.net.URL

import cats.effect.IO
import cats.implicits._
import com.flowtick.graphs._
import com.flowtick.graphs.graphml.GraphML
import scalafx.application.JFXApp
import scalafx.scene.layout.BorderPane
import scalafx.scene.{Group, Scene}

object EditorMainJvm extends JFXApp with EditorMain {

  lazy val pageRoot = new Group

  lazy val editorLayout = new BorderPane {
    center = pageRoot
    style = s"""-fx-background: #DDD;""".stripMargin
  }

  lazy val editorScene = new Scene(editorLayout) {
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
    home <- sys.props.get("user.home").map { userHome =>
      IO(new File(userHome))
    }.getOrElse(IO.raiseError(new IllegalStateException("could not find user home")))

    loadOptions <- IO {
      val graphsDir = new File(home, ".graphs")
      graphsDir.mkdir()
      val configFile = new File(graphsDir, "config.json")
      if (configFile.exists()) {

        val configSource = scala.io.Source.fromFile(configFile)
        val configContent = configSource.getLines().mkString("\n")
        configSource.close()

        // TODO: proper encoding of options

        import io.circe.generic.auto._
        import json.format.default._
        import com.flowtick.graphs.json.schema.JsonSchema._

        io.circe.parser.decode[EditorOptions](configContent)
      } else Right(EditorOptions())
    }
    options <- IO.fromEither(loadOptions)
    _ <- options.palette.toList.flatMap(_.images).map {
      case (ref, image) => ImageLoader.registerImage(ref, image)
    }.sequence

    editor <- createEditor(bus => List(
      new EditorMenuJavaFx(bus, editorLayout, stage),
      new EditorViewJavaFx(bus, editorLayout),
      new EditorPaletteJavaFx(bus, editorLayout),
      new EditorPropertiesJavaFx(bus, editorLayout)
    ))(options)
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