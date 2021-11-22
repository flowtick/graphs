package com.flowtick.graphs.editor
import cats.effect.IO
import com.flowtick.graphs.editor.vendor.{Ace, AceEditor}
import io.circe.Json
import org.scalajs.dom.Element
import org.scalajs.dom.experimental.Fullscreen
import org.scalajs.dom.html.{Div, Form, TextArea}
import org.scalajs.dom.raw.{Event, HTMLElement}
import scalatags.{JsDom, generic}

import scala.scalajs.js
import cats.implicits._
import com.flowtick.graphs.editor.EditorProperties.eventHandler

final case class PropertyControlJs(
    property: PropertySpec,
    container: JsDom.TypedTag[Div],
    init: IO[Unit],
    set: Json => IO[Unit]
) extends PropertyControl

final case class PropertyForm(controls: List[PropertyControlJs], html: Form)

object EditorPropertiesHtml {
  import scalatags.JsDom.all._

  lazy val propertiesToggle = button(
    `type` := "button",
    cls := "btn btn-primary",
    data("toggle") := "modal",
    data("target") := "#properties-panel",
    display := "none"
  ).render

  def createPropertyForm(properties: List[PropertySpec]): IO[PropertyForm] = for {
    controls <- properties.sortBy(_.order).map(propertyControl).sequence
    propertyFormHtml = form(
      id := "properties"
    ).apply(controls.map(_.container): _*).render
  } yield PropertyForm(controls, propertyFormHtml)

  def propertyContainers(
      property: PropertySpec,
      input: HTMLElement
  ): (Div, JsDom.TypedTag[Div]) = {
    val propertyId = property.key
      .flatMap(_.name)
      .getOrElse(property.title)
      .replaceAll(" ", "_")
      .toLowerCase

    val optionalClass: Option[generic.AttrPair[Element, String]] =
      if (property.collapsable.contains(false)) Some(cls := "d-none") else None

    lazy val inputContainer: Div = div(
      optionalClass,
      id := s"property_container_$propertyId",
      input
    ).render

    def fullScreenOnDoubleClick(element: HTMLElement) =
      ondblclick := ((e: Event) => {
        Fullscreen.toFullscreenElement(element).requestFullscreen()
      })

    lazy val showButton = a(
      cls := "btn btn-secondary",
      span(i(cls := "fas fa-search"), property.title),
      onclick := ((e: Event) => {
        inputContainer.classList.remove("d-none")
      }),
      fullScreenOnDoubleClick(inputContainer)
    )

    (
      inputContainer,
      div(
        cls := s"form-group",
        label(
          `for` := propertyId,
          property.collapsable
            .map(_ => showButton)
            .getOrElse(
              span(property.title, fullScreenOnDoubleClick(inputContainer))
            )
        ),
        title := property.description.getOrElse(""),
        inputContainer
      )
    )
  }

  def propertyKeyString(property: PropertySpec): String =
    property.key.flatMap(_.name).getOrElse(property.title)

  def propertyControl(property: PropertySpec): IO[PropertyControlJs] =
    property.inputType match {
      case NumberInput =>
        lazy val numberInput = input(
          id := s"${propertyKeyString(property)}_number",
          cls := s"form-control",
          `type` := "number",
          step := "0.1"
        ).render

        val (_, propertyContainer) = propertyContainers(property, numberInput)

        IO(
          PropertyControlJs(
            property,
            propertyContainer,
            init = IO {
              numberInput.onchange = eventHandler[Event] { _ =>
                property.handler(
                  JsonValue(Json.fromDoubleOrString(numberInput.value.toDouble))
                )
              }
            },
            set = json =>
              IO(numberInput.value =
                NumberInput
                  .fromJson(property.key, json)
                  .map(_.toString)
                  .getOrElse("")
              )
          )
        )

      case BooleanInput =>
        lazy val checkbox = input(
          id := s"${propertyKeyString(property)}_boolean",
          cls := s"form-control",
          `type` := "checkbox"
        ).render

        val (_, propertyContainer) = propertyContainers(property, checkbox)

        IO(
          PropertyControlJs(
            property,
            propertyContainer,
            init = IO {
              checkbox.onchange = eventHandler[Event](_ =>
                property.handler(
                  JsonValue(if (checkbox.checked) Json.True else Json.False)
                )
              )
            },
            set = json =>
              IO(checkbox.checked = BooleanInput.fromJson(property.key, json).getOrElse(false))
          )
        )

      case IntegerInput =>
        lazy val integerInput = input(
          id := s"${propertyKeyString(property)}_integer",
          cls := s"form-control",
          `type` := "number",
          step := 1,
          oninput := "this.value=(parseInt(this.value)||0)",
          pattern := "[0-9]"
        ).render

        val (_, propertyContainer) = propertyContainers(property, integerInput)

        IO(
          PropertyControlJs(
            property,
            propertyContainer,
            init = IO {
              integerInput.onchange = eventHandler[Event](_ =>
                property.handler(
                  JsonValue(Json.fromInt(integerInput.value.toInt))
                )
              )
            },
            set = json =>
              IO(integerInput.value =
                IntegerInput
                  .fromJson(property.key, json)
                  .map(_.toString)
                  .getOrElse("")
              )
          )
        )

      case TextInput | LabelInputType | JsonInputType =>
        lazy val textAreaInput: TextArea = textarea(
          id := s"${propertyKeyString(property)}_text",
          cls := s"form-control",
          onchange :=
            eventHandler[Event](_ => {
              val newValue = textAreaInput.value
              property.handler(JsonValue(Json.fromString(newValue)))
            })
        ).render

        val (inputContainer, propertyContainer) =
          propertyContainers(property, textAreaInput)

        lazy val createEditor: IO[Either[TextArea, AceEditor]] =
          property.highlight match {
            case Some(mode) =>
              IO {
                val editor = Ace.edit(inputContainer)
                editor.on(
                  "blur",
                  (_: js.UndefOr[js.Object]) => {
                    property.inputType match {
                      case JsonInputType =>
                        io.circe.parser.decode[Json](editor.getValue()) match {
                          case Right(json) =>
                            eventHandler[Unit](_ => property.handler(JsonValue(json))).apply()
                          case Left(error) => println(error)
                        }
                      case _ =>
                        eventHandler[Unit](_ =>
                          property.handler(
                            JsonValue(Json.fromString(editor.getValue()))
                          )
                        ).apply()
                    }
                  }
                )
                editor.setTheme("ace/theme/github")
                editor.session.setMode(s"ace/mode/$mode")
                editor
              }.attempt.flatMap {
                case Right(editor) => IO.pure(Right(editor))
                case Left(error) =>
                  IO {
                    println(
                      s"unable to set ace mode $mode:, falling back to textarea",
                      error
                    )
                    Left(textAreaInput)
                  }
              }

            case None =>
              IO {
                Left(textAreaInput)
              }
          }

        createEditor.map(editor =>
          PropertyControlJs(
            property,
            propertyContainer,
            init = IO.unit,
            set = json =>
              IO {
                val newValue = TextInput.fromJson(property.key, json).getOrElse("")
                editor match {
                  case Right(ace) =>
                    ace.session.setValue(newValue)
                  case Left(text) =>
                    text.value = newValue
                    text.rows = newValue.split("\n").length
                }
              }
          )
        )

      case other =>
        IO(
          PropertyControlJs(
            property,
            div(
              pre(s"unsupported property type $other", style := "display: none")
            ),
            IO.unit,
            _ => IO.unit
          )
        )
    }

  lazy val propertiesBody = div(
    cls := "modal-body"
  ).render

  lazy val propertiesPanel =
    div(
      cls := "modal fade",
      id := "properties-panel",
      tabindex := "-1",
      role := "dialog",
      aria.labelledby := "exampleModalLabel3",
      aria.hidden := true,
      div(
        cls := "modal-dialog modal-dialog-slideout",
        role := "document",
        div(
          cls := "modal-content",
          div(
            cls := "modal-header",
            h5(cls := "modal-title", id := "exampleModalLabel", "Properties"),
            button(
              `type` := "button",
              cls := "close",
              data("dismiss") := "modal",
              aria.label := "close",
              span(aria.hidden := "true", "×")
            )
          ),
          propertiesBody,
          div(
            cls := "modal-footer",
            button(
              `type` := "button",
              cls := "btn btn-secondary",
              data("dismiss") := "modal",
              "Close"
            )
          )
        )
      )
    ).render

}
