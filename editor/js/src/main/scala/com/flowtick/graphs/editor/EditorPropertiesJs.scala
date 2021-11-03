package com.flowtick.graphs.editor

import cats.effect.IO

class EditorPropertiesJs(containerId: String)(val messageBus: EditorMessageBus)
    extends EditorProperties {

  lazy val container =
    org.scalajs.dom.window.document.getElementById(containerId)

  override def initEditor(model: EditorModel): IO[Unit] = IO {
    container.parentNode.appendChild(EditorPropertiesHtml.propertiesPanel)
    container.parentNode.appendChild(EditorPropertiesHtml.propertiesToggle)
  }

  override def toggleEdit(enabled: Boolean): IO[Boolean] =
    IO(EditorPropertiesHtml.propertiesToggle.click()).attempt.map(_ => true)

  override def setPropertiesGroups(
      properties: List[PropertySpec],
      elementProperties: ElementProperties
  ): IO[List[PropertyFormGroup]] = for {
    newForm <- EditorPropertiesHtml.createPropertyForm(properties)
    _ <- IO {

      if (EditorPropertiesHtml.propertiesBody.children.length == 0) {
        EditorPropertiesHtml.propertiesBody.appendChild(newForm.html)
      } else {
        EditorPropertiesHtml.propertiesBody.replaceChild(
          newForm.html,
          EditorPropertiesHtml.propertiesBody.firstChild
        )
      }
    }
  } yield newForm.groups
}
