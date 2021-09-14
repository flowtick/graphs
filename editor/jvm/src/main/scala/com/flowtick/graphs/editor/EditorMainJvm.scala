package com.flowtick.graphs.editor

import java.io.File
import java.net.URL

import cats.effect.IO
import scalafx.application.JFXApp
import scalafx.scene.image.Image
import scalafx.scene.layout.BorderPane
import scalafx.scene.{Group, Scene}

object EditorMainJvm extends JFXApp with EditorMain {

  lazy val pageRoot = new Group

  lazy val editorLayout = new BorderPane {
    center = pageRoot
    style = s"""-fx-background: #DDD;""".stripMargin
  }

  lazy val editorScene = new Scene(editorLayout) {
    val stylePath = sys.env.getOrElse(
      "GRAPHS_THEME",
      getClass.getClassLoader.getResource("style.css").toURI.toString
    )
    stylesheets.add(stylePath)
  }

  stage = new JFXApp.PrimaryStage {
    title.value = "graphs editor"
    width = 600
    height = 450
    scene = editorScene
  }

  def initEditor: IO[EditorInstance] = for {
    home <- sys.props
      .get("user.home")
      .map { userHome =>
        IO(new File(userHome))
      }
      .getOrElse(
        IO.raiseError(new IllegalStateException("could not find user home"))
      )

    configFile <- IO {
      sys.env.get("GRAPHS_CONFIG").map(new File(_)).getOrElse {
        val graphsDir = new File(home, ".graphs")
        graphsDir.mkdir()
        new File(graphsDir, "config.json")
      }
    }

    config <-
      if (configFile.exists()) {
        IO(scala.io.Source.fromFile(configFile)).bracket { configSource =>
          val configContent = configSource.getLines().mkString("\n")
          IO.fromEither(EditorConfiguration.decode(configContent))
            .attempt
            .flatMap {
              case Right(config) => IO.pure(config)
              case Left(error) =>
                IO(println(s"unable to load config: $configContent")) *> IO
                  .raiseError(error)
            }
        }(source => IO(source.close()))
      } else
        IO(println(s"config not found: $configFile")) *> IO.pure(
          EditorConfiguration()
        )

    editor <- createEditor(bus =>
      List(
        new EditorMenuJavaFx(bus, editorLayout, stage),
        new EditorViewJavaFx(bus, editorLayout),
        new EditorPaletteJavaFx(bus, editorLayout),
        new EditorPropertiesJavaFx(bus, editorLayout),
        new EditorImageLoader[Image](ImageLoaderFx)
      )
    )(config)
    _ <- parameters.raw.headOption match {
      case Some(firstArg) =>
        for {
          fileUrl <- IO(new URL(firstArg)).redeemWith(
            _ => IO(new URL(s"file://${new File(firstArg).getAbsolutePath}")),
            IO.pure
          )
          _ <- IO(scala.io.Source.fromURL(fileUrl)).bracket { source =>
            val format =
              if (fileUrl.getFile.endsWith(".json")) JsonFormat
              else GraphMLFormat
            editor.bus.publish(Load(source.getLines().mkString("\n"), format))
          }(source => IO(source.close()))
        } yield ()
      case None => IO.unit
    }
  } yield editor

  initEditor.unsafeRunSync()
}
