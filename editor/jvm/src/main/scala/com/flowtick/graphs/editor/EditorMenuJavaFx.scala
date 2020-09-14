package com.flowtick.graphs.editor

import java.io.{FileInputStream, FileOutputStream}

import cats.effect.IO
import com.flowtick.graphs._
import javafx.event.EventHandler
import javafx.scene.input.KeyEvent
import scalafx.scene.control.{Menu, MenuBar, MenuItem}
import scalafx.scene.input.{KeyCode, KeyCodeCombination, KeyCombination}
import scalafx.scene.layout.BorderPane
import scalafx.stage.{FileChooser, Stage}

class EditorMenuJavaFx(val messageBus: EditorMessageBus, layout: BorderPane, stage: Stage) extends EditorMenu {

  lazy val menu = {
    val bar = new MenuBar {
      menus = editorMenus.map { menuSpec =>
        new Menu(menuSpec.title) {
          items = menuSpec.actions.map(action => {
            new MenuItem(action.title) {
              onAction = event => {
                event.consume()
                action.handler(event)
              }
            }
          })
        }
      }
    }
    bar.setViewOrder(-1.0) // lowest comes first, always shows menu
    bar
  }

  def openFile: IO[Unit] = IO {
    val fileChooser = new FileChooser()
    fileChooser.setTitle("Open File")

    val graphmlFilter = new FileChooser.ExtensionFilter("graphml files (*.graphml)", "*.graphml")
    val jsonFilter = new FileChooser.ExtensionFilter("JSON files (*.json)", "*.json")

    fileChooser.getExtensionFilters.add(graphmlFilter)
    fileChooser.getExtensionFilters.add(jsonFilter)

    Option(fileChooser.showOpenDialog(stage))
  }.attempt.flatMap {
    case Right(Some(file)) => {
      val format = file.getAbsolutePath.split("\\.").lastOption match {
        case Some("json") => JsonFormat
        case _ => GraphMLFormat
      }
      loadGraph(file.getAbsolutePath, format)
    }
    case Right(None) => IO(println("no file selected"))
    case Left(error) => IO.raiseError(error)
  }

  case class ShortCut(keyCode: Option[KeyCode] = None, modifiers: List[KeyCombination.Modifier] = List.empty) {
    def toCombination: Option[KeyCodeCombination] = keyCode.map(code => new KeyCodeCombination(code, modifiers.map(_.delegate): _*))
  }

  override def bindShortcut(action: Action): IO[Unit] = IO {
    val matchingShortCut = action.shortCut.split("\\+").foldLeft[ShortCut](ShortCut()) {
      case (shortcut, nextPart) => nextPart match {
        case "alt" => shortcut.copy(modifiers = shortcut.modifiers.appended(KeyCombination.AltDown))
        case "ctrl" => shortcut.copy(modifiers = shortcut.modifiers.appended(KeyCombination.ControlDown))
        case "shift" => shortcut.copy(modifiers = shortcut.modifiers.appended(KeyCombination.ShiftDown))
        case "ins" => shortcut.copy(keyCode = Some(KeyCode.Insert))
        case "del" => shortcut.copy(keyCode = Some(KeyCode.Delete))
        case other => shortcut.copy(keyCode = KeyCode.values.find(_.name.toLowerCase == other.toLowerCase))
      }
    }

    matchingShortCut.toCombination.foreach { combination =>
      val currentHandler = Option[EventHandler[_ >: KeyEvent]](stage.scene.value.getOnKeyReleased)
      stage.scene.value.setOnKeyReleased((event) => {
        if(combination.`match`(event)) {
          println(s"combination $combination")
          event.consume()
          action.handler(event)
        } else {
          currentHandler.foreach(_.handle(event))
        }
      })
    }
  }

  override def triggerFileOpen: Any => Unit = _ => {
    openFile.unsafeRunSync()
  }

  override def handleExported(exported: ExportedGraph): IO[Unit] = for {
    file <- IO {
      new FileChooser(){
        title = s"Save as ${exported.format.`extension`}"
      }.showSaveDialog(stage)
    }.map(Option(_))
    _ <- file match {
      case Some(path) => IO {
        val out = new FileOutputStream(path)
        out.write(exported.value.getBytes("UTF-8"))
        out.flush()
        out.close()
      }
      case None => IO.unit
    }
  } yield ()

  override def initMenus: IO[Unit] = IO {
    layout.setTop(menu)
  }

  def loadGraph(path: String, format: FileFormat): IO[Unit] = for {
    fileContent <- IO {
      scala.io.Source
        .fromInputStream(new FileInputStream(path))
        .getLines()
        .mkString("\n")
    }
    _ <- messageBus.publish(Load(fileContent, format))
  } yield ()

}
