package com.flowtick.graphs.editor
import cats.effect.IO
import com.flowtick.graphs.editor.vendor.{Ace, AceEditor}
import io.circe.Json
import org.scalajs.dom.experimental.Fullscreen
import org.scalajs.dom.html.{Div, Element, Form, TextArea}
import org.scalajs.dom.raw.{Event, HTMLElement}
import scalatags.JsDom

import scala.scalajs.js

final case class PropertyFormGroupJs(property: PropertySpec,
                                     container: JsDom.TypedTag[Div],
                                     init: () => Unit,
                                     set: Json => Unit) extends PropertyFormGroup

final case class PropertyForm(groups: List[PropertyFormGroupJs], html: Form)

object EditorPropertiesHtml {
  import scalatags.JsDom.all._

  lazy val propertiesToggle = button(
    `type` := "button",
    cls := "btn btn-primary",
    data("toggle") := "modal",
    data("target") := "#properties-panel",
    display := "none"
  ).render

  def createPropertyForm(properties: List[PropertySpec]): PropertyForm = {
    val groups = properties.sortBy(_.order).map(propertyGroup)

    val propertyFormHtml = form(
      id := "properties",
    ).apply(groups.map(_.container): _*).render

    PropertyForm(groups, propertyFormHtml)
  }

  def propertyContainers(property: PropertySpec, input: HTMLElement): (Div, JsDom.TypedTag[Div]) = {
    val propertyId = property.key.getOrElse(property.title).replaceAll(" ", "_").toLowerCase

    lazy val inputContainer: Div = div(
      cls := (if(property.collapsable.contains(false)) "d-none" else ""),
      id := s"property_container_$propertyId",
      input
    ).render

    def fullScreenOnDoubleClick(element: HTMLElement) = ondblclick := ((e: Event) => {
      Fullscreen.toFullscreenElement(element).requestFullscreen()
    })

    lazy val showButton = a(
      cls := "btn btn-secondary", span(i(cls :="fas fa-search"), property.title),
      onclick := ((e: Event) => {
        inputContainer.classList.remove("d-none")
      }),
      fullScreenOnDoubleClick(inputContainer)
    )

    (inputContainer, div(
      cls := s"form-group",
      label(`for` := propertyId, property.collapsable.map(_ => showButton).getOrElse(span(property.title, fullScreenOnDoubleClick(inputContainer)))),
      title := property.description.getOrElse(""),
      inputContainer
    ))
  }

  def propertyGroup(property: PropertySpec): PropertyFormGroupJs = property.inputType match {
    case NumberInput =>
      lazy val numberInput = input(
        id := s"${property.key}_number",
        cls := s"form-control",
        `type` := "number",
        step := "0.1"
      ).render

      val (_, propertyContainer) = propertyContainers(property, numberInput)

      PropertyFormGroupJs(property, propertyContainer,
        init = () => {
          numberInput.onchange = _ => property.handler(Json.fromDoubleOrString(numberInput.value.toDouble))
        },
        set = json => numberInput.value = NumberInput.fromJson(property.key, json).map(_.toString).getOrElse("")
      )

    case BooleanInput =>
      lazy val checkbox = input(
        id := s"${property.key}_boolean",
        cls := s"form-control",
        `type` := "checkbox"
      ).render

      val (_, propertyContainer) = propertyContainers(property, checkbox)

      PropertyFormGroupJs(property, propertyContainer, init = () => {
        checkbox.onchange = _ => property.handler(if(checkbox.checked) Json.True else Json.False)
      }, set = json => checkbox.checked = BooleanInput.fromJson(property.key, json).getOrElse(false))

    case IntegerInput =>
      lazy val integerInput = input(
        id := s"${property.key}_integer",
        cls := s"form-control",
        `type` := "number",
        step := 1,
        oninput := "this.value=(parseInt(this.value)||0)",
        pattern := "[0-9]"
      ).render

      val (_, propertyContainer) = propertyContainers(property, integerInput)

      PropertyFormGroupJs(property, propertyContainer, init = () => {
        integerInput.onchange = _ => property.handler(Json.fromInt(integerInput.value.toInt))
      }, set = json => integerInput.value = IntegerInput.fromJson(property.key, json).map(_.toString).getOrElse(""))

    case TextInput | LabelInputType | JsonInputType =>
      lazy val textAreaInput: TextArea = textarea(
        id := s"${property.key}_text",
        cls := s"form-control"
      ).render

      val (inputContainer, propertyContainer) = propertyContainers(property, textAreaInput)

      lazy val textAreaInit = {
        textAreaInput.onchange = _ => property.handler(Json.fromString(textAreaInput.value))
      }

      lazy val editor: Either[TextArea, AceEditor] = (property.highlight match {
        case Some(mode) =>
          IO {
            val editor = Ace.edit(inputContainer.id)
            editor.on("blur", (_: js.UndefOr[js.Object]) =>  {
              property.inputType match {
                case JsonInputType =>
                  io.circe.parser.decode[Json](editor.getValue()) match {
                    case Right(json) => property.handler(json)
                    case Left(error) => println(error)
                  }
                case _ =>  property.handler(Json.fromString(editor.getValue()))
              }
            })
            editor.setTheme("ace/theme/github")
            editor.session.setMode(s"ace/mode/$mode")
            editor
          }.attempt.flatMap {
            case Right(editor) => IO.pure(Right(editor))
            case Left(error) =>
              IO {
                println(s"unable to set ace mode $mode, falling back to textarea")
                println(error)
                textAreaInit
                Left(textAreaInput)
              }
          }

        case None => IO {
          textAreaInit
          Left(textAreaInput)
        }
      }).unsafeRunSync()

      PropertyFormGroupJs(property, propertyContainer, init = () => editor, set = json => {
        val newValue = TextInput.fromJson(property.key, json).getOrElse("")
        editor match {
          case Right(ace) => ace.session.setValue(newValue)
          case Left(text) =>
            text.value = newValue
            text.rows = newValue.split("\n").length
        }
      })
  }

  lazy val propertiesBody = div(
    cls := "modal-body",
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
            cls :="modal-header",
            h5(cls := "modal-title", id := "exampleModalLabel", "Properties"),
            button(`type` := "button", cls := "close", data("dismiss") := "modal", aria.label := "close", span(aria.hidden := "true", "×"))
          ),
          propertiesBody,
          div(
            cls :="modal-footer",
            button(`type` := "button", cls := "btn btn-secondary", data("dismiss") := "modal", "Close")
          )
        )
      )
    ).render


}
