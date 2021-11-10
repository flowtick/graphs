package com.flowtick.graphs.editor
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import io.circe.Json
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.scene.paint.Color
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control._
import scalafx.scene.layout._
import scalafx.scene.text.{Font, FontWeight}

case class PropertyFormGroupFx(
    property: PropertySpec,
    init: IO[Unit],
    set: Json => IO[Unit]
) extends PropertyFormGroup

class EditorPropertiesJavaFx(
    val messageBus: EditorMessageBus,
    layout: BorderPane
) extends EditorProperties {
  val pane = new AnchorPane() {
    visible = false
    background = new Background(
      Array(
        new BackgroundFill(
          scalafx.scene.paint.Color.LightGray,
          new CornerRadii(0),
          Insets.Empty
        )
      )
    )

    viewOrder_(-10)

    minWidth = 300
  }

  override def initEditor(editorModel: EditorModel): IO[Unit] = IO {
    layout.right = pane
    // DragResizer.makeResizable(pane)
  }

  override def toggleEdit(enabled: Boolean): IO[Boolean] = IO {
    pane.visible = enabled
    enabled
  }

  override protected def setPropertiesGroups(
      properties: List[PropertySpec],
      elementProperties: ElementProperties
  ): IO[List[PropertyFormGroup]] = for {
    groups <- IO {

      pane.children.clear()

      val closeLabel = new Label("X") {
        minHeight = 30.0
        font = Font.apply("Arial", FontWeight.Bold, 20.0)
      }

      closeLabel.onMouseClicked = _ => {
        messageBus
          .publish(EditorToggle(EditorToggle.editKey, Some(false)))
          .unsafeToFuture()
      }

      val grid = new GridPane()
      grid.setHgap(10)
      grid.setVgap(10)
      grid.setPadding(Insets(0, 10, 0, 10))
      grid.setAlignment(Pos.Center)

      AnchorPane.setTopAnchor(grid, 10.0)
      AnchorPane.setLeftAnchor(grid, 10.0)

      grid.add(closeLabel, 2, 0)
      pane.children.add(grid)

      properties.sortBy(_.order).zipWithIndex.map { case (property, index) =>
        propertyGroup(property, index + 1)(grid)
      }
    }
  } yield groups

  def label(property: PropertySpec): Label = {
    val label = new Label(property.title) {}
    val tooltip = new Tooltip {
      text = property.description.getOrElse("")
    }
    label.setTooltip(tooltip)
    label
  }

  def propertyGroup(property: PropertySpec, index: Int)(
      grid: GridPane
  ): PropertyFormGroupFx = property.inputType match {
    case NumberInput =>
      lazy val input = new TextField()

      PropertyFormGroupFx(
        property,
        init = IO {
          grid.add(label(property), 0, index)
          grid.add(input, 1, index)

          input.text.addListener(new ChangeListener[String] {
            override def changed(
                observableValue: ObservableValue[_ <: String],
                oldValue: String,
                newValue: String
            ): Unit = {
              property.handler(
                JsonValue(Json.fromDoubleOrString(newValue.toDouble))
              )
            }
          })
        },
        set = (newJson: Json) =>
          IO {
            val newText = NumberInput
              .fromJson(property.key, newJson)
              .map(_.toString)
              .getOrElse("")
            input.setText(newText)
          }
      )

    case BooleanInput =>
      lazy val input = new CheckBox()

      PropertyFormGroupFx(
        property,
        init = IO {
          grid.add(label(property), 0, index)
          grid.add(input, 1, index)

          input.selected.addListener(new ChangeListener[java.lang.Boolean] {
            override def changed(
                observableValue: ObservableValue[_ <: java.lang.Boolean],
                oldValue: java.lang.Boolean,
                newValue: java.lang.Boolean
            ): Unit = {
              property.handler(JsonValue(Json.fromBoolean(newValue)))
            }
          })
        },
        set = (newJson: Json) =>
          IO {
            input.selected = BooleanInput.fromJson(property.key, newJson).getOrElse(false)
          }
      )

    case IntegerInput =>
      lazy val input = new TextField()

      PropertyFormGroupFx(
        property,
        init = IO {
          grid.add(label(property), 0, index)
          grid.add(input, 1, index)

          input.text.addListener(new ChangeListener[String] {
            override def changed(
                observableValue: ObservableValue[_ <: String],
                oldValue: String,
                newValue: String
            ): Unit = {
              property.handler(JsonValue(Json.fromInt(newValue.toInt)))
            }
          })
        },
        set = (newJson: Json) =>
          IO {
            val newText = IntegerInput
              .fromJson(property.key, newJson)
              .map(_.toString)
              .getOrElse("")
            input.setText(newText)
          }
      )

    case ColorInputType =>
      val input = new ColorPicker()

      PropertyFormGroupFx(
        property,
        init = IO {
          grid.add(label(property), 0, index)
          grid.add(input, 1, index)

          input.value.addListener(new ChangeListener[Color] {
            override def changed(
                observableValue: ObservableValue[_ <: Color],
                oldColor: Color,
                newColor: Color
            ): Unit = {
              property.handler(ColorValue(newColor.toString))
            }
          })
        },
        (newJson: Json) =>
          IO {
            input.value = newJson.asString.map(Color.web).getOrElse(Color.WHITE)
          }
      )

    case _ =>
      val input = new TextArea()

      PropertyFormGroupFx(
        property,
        IO {
          grid.add(label(property), 0, index)
          grid.add(input, 1, index)

          input.text.addListener(new ChangeListener[String] {
            override def changed(
                observableValue: ObservableValue[_ <: String],
                oldValue: String,
                newValue: String
            ): Unit = {
              if (property.inputType == JsonInputType) {
                io.circe.parser.decode[Json](newValue) match {
                  case Right(json) => property.handler(JsonValue(json))
                  case Left(_)     =>
                }
              } else property.handler(JsonValue(Json.fromString(newValue)))
            }
          })
        },
        (newJson: Json) =>
          IO {
            val newText = if (property.inputType == JsonInputType) {
              newJson.spaces2
            } else TextInput.fromJson(property.key, newJson).getOrElse("")

            val rows = newText.split("\n").length
            input.text.value = newText
            input.setPrefRowCount(if (rows == 0) 1 else rows)
          }
      )
  }

}
