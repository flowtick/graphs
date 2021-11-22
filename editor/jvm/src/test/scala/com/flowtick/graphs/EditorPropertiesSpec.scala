package com.flowtick.graphs
import cats.implicits._
import cats.effect.IO
import cats.effect.kernel.Ref
import cats.effect.unsafe.implicits.global
import com.flowtick.graphs.editor._
import com.flowtick.graphs.view.{ElementRef, NodeElementType}
import io.circe.Json
import org.apache.logging.log4j.{LogManager, Logger}

final case class TestControl(property: PropertySpec, value: Ref[IO, Option[Json]], init: IO[Unit])
    extends PropertyControl {
  protected val log: Logger = LogManager.getLogger(getClass)

  override def set: Json => IO[Unit] = json =>
    IO(
      log.info(s"test: setting ${property.key.flatMap(_.name).getOrElse(property.title)} to $json")
    ) *>
      value.set(Some(json))
}

class TestProperties(bus: EditorMessageBus) extends EditorProperties {
  protected val log: Logger = LogManager.getLogger(getClass)

  override def messageBus: EditorMessageBus = bus
  override def initEditor(editorModel: EditorModel): IO[Unit] = IO.unit
  override def toggleEdit(enabled: Boolean): IO[Boolean] =
    IO(log.debug(s"toggle edit $enabled")) *> IO.pure(enabled)

  override protected def createPropertyControls(
      properties: List[PropertySpec],
      values: EditorPropertiesValues
  ): IO[List[PropertyControl]] =
    IO(properties.map { prop =>
      TestControl(
        prop,
        value = Ref.unsafe(None),
        init = IO(log.info(s"test: init $prop"))
      )
    })
}

class EditorPropertiesSpec extends EditorBaseSpec {
  override def testComponents(bus: EditorMessageBus): List[EditorComponent] = List(
    new TestProperties(bus)
  )

  "Editor Properties" should "set values" in withEditor { editor =>
    def getControls: IO[List[PropertyControl]] = editor.components
      .traverse {
        case properties: TestProperties => properties.currentControls.get
        case _                          => IO.pure(List.empty)
      }
      .map(_.flatten)

    (for {
      _ <- editor.bus.publish(Reset)
      _ <- editor.bus.publish(
        AddNode("1", stencilRef = Some("test"), Some(100.0), Some(100.0))
      )
      elementRef = ElementRef("1", NodeElementType)
      controlsBeforeSelect <- getControls
      _ = controlsBeforeSelect should be(empty)

      _ <- editor.bus.publish(Select(Set(elementRef)))

      controlsAfterSelect <- getControls

      textControl <- IO.fromOption(controlsAfterSelect.find(_.property.inputType == TextInput))(
        new IllegalStateException("text control should exist")
      )
      _ <- editor.bus.publish(SetJson(elementRef, _ => Json.fromString("new Text")))
      textValue <- textControl match {
        case testControl: TestControl => testControl.value.get
        case _ =>
          IO.raiseError(new IllegalStateException("only test controls expected during test"))
      }
      _ = textValue should be(Some(Json.fromString("new Text")))
    } yield ()).unsafeToFuture().futureValue
  }
}
