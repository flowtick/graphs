package com.flowtick.graphs.editor

import cats.effect.IO
import cats.effect.concurrent.Ref
import com.flowtick.graphs.{ElementRef, json, _}
import com.flowtick.graphs.graphml.{GraphMLElement, GraphMLGraph}
import com.flowtick.graphs.json.schema.Schema
import io.circe.Json
import cats.implicits._

import scala.util.Try

final case class ElementProperties(element: ElementRef,
                                   labelValue: Option[String],
                                   elementSchema: EditorModel.EditorSchema,
                                   value: Json)

sealed trait PropertyInputType

case object LabelInputType extends PropertyInputType
case object JsonInputType extends PropertyInputType

case object TextInput extends PropertyInputType {
  def fromJson(key: Option[String], json: Json): Option[String] = {
    key.map(keyValue => json.hcursor.downField(keyValue)
      .focus
      .flatMap(_.asString)).getOrElse(json.asString)
  }
}

case object IntegerInput extends PropertyInputType {
  def fromJson(key: Option[String], json: Json): Option[Int] = {
    key.map(keyValue => json.hcursor.downField(keyValue)
      .focus
      .flatMap(_.asNumber)).getOrElse(json.asNumber).flatMap(_.toInt)
  }
}

case object BooleanInput extends PropertyInputType {
  def fromJson(key: Option[String], json: Json): Option[Boolean] = {
    key.map(keyValue => json.hcursor.downField(keyValue)
      .focus
      .flatMap(_.asBoolean)).getOrElse(json.asBoolean)
  }
}
case object NumberInput extends PropertyInputType {
  def fromJson(key: Option[String], json: Json): Option[Double] = {
    key.map(keyValue => json.hcursor.downField(keyValue)
      .focus
      .flatMap(_.asNumber)).getOrElse(json.asNumber)
      .map(_.toDouble)
  }
}

final case class PropertySpec(key: Option[String] = None, // no key means its the root value
                              title: String,
                              inputType: PropertyInputType,
                              handler: Json => Unit,
                              description: Option[String] = None,
                              collapsable: Option[Boolean] = None,
                              highlight: Option[String] = None,
                              order: Float = 0.5f)

class EditorPropertiesJs(containerId: String)(messageBus: EditorMessageBus) extends EditorComponent {

  lazy val currentElement: Ref[IO, Option[ElementRef]] = Ref.unsafe(None)
  lazy val currentProperties: Ref[IO, List[PropertyFormGroup]] = Ref.unsafe(List.empty)

  lazy val container = org.scalajs.dom.window.document.getElementById(containerId)

  override def init(model: EditorModel): IO[Unit] = IO {
    container.parentNode.appendChild(EditorPropertiesHtml.propertiesPanel)
    container.parentNode.appendChild(EditorPropertiesHtml.propertiesToggle)
  }

  def handleInputValue(f: Json => ElementRef => Vector[EditorEvent]): Json => Unit = (value: Json) => (for {
    elem <- currentElement.get
    _ <- elem match {
      case Some(ref) =>
        IO(f(value)(ref)).flatMap { events =>
          events.map {
            case command: EditorCommand => messageBus.publish(command)
            case other => messageBus.notifyEvent(this, other)
          }.sequence
        }
      case None => IO.unit
    }
  } yield ()).unsafeRunSync()

  override lazy val eval: Eval = ctx => ctx.effect(this) {
    case Selected(elements, _) =>
      elements
        .headOption
        .flatMap(getElementProperties(_, ctx.model.graphml, ctx.model.schema))
        .map(setElementAndReset)
        .getOrElse(IO.unit)

    case ElementUpdated(updatedElement, _) => for {
      current <- currentElement.get
      _ <- if (!current.contains(updatedElement)) {
        getElementProperties(updatedElement, ctx.model.graphml, ctx.model.schema)
          .map(setElementAndReset)
          .getOrElse(IO.unit)
      } else current
        .flatMap(getElementProperties(_, ctx.model.graphml, ctx.model.schema))
        .map(updateValues)
        .getOrElse(IO.unit)
    } yield ()

    case Toggle(Toggle.editKey, true) => for {
      _ <- IO(EditorPropertiesHtml.propertiesToggle.click()).attempt
    } yield ()
  }

  private def updateValues(elementProperties: ElementProperties): IO[Unit] = for {
    properties <- currentProperties.get
    _ <- properties.map(setGroupValues(_, elementProperties)).sequence
  } yield ()

  private def setGroupValues(group: PropertyFormGroup, elementProperties: ElementProperties) = IO {
    group.property.inputType match {
      case LabelInputType => group.set(Json.fromString(elementProperties.labelValue.getOrElse("")))
      case JsonInputType => group.set(Json.fromString(elementProperties.value.spaces2))
      case _ => group.set(elementProperties.value)
    }
  }

  private def setElementAndReset(elementProperties: ElementProperties): IO[Unit] = {
    for {
      _ <- currentElement.set(Some(elementProperties.element))
      _ <- resetPanel(elementProperties)
    } yield ()
  }

  def resetPanel(elementProperties: ElementProperties): IO[Unit] = {
      val commonProperties: List[PropertySpec] = List(
        Some(PropertySpec(
          title = "Label",
          order = 0.0f,
          inputType = LabelInputType,
          handler = handleInputValue(text => ref => Vector(SetLabel(ref, text.asString.getOrElse("")))))
        ).filterNot(_ => elementProperties.elementSchema.extension.exists(_.hideLabelProperty.contains(true))),
        Some(PropertySpec(
          title = "Json",
          order = 1.0f,
          inputType = JsonInputType,
          description = Some("Set the JSON value directly, note: this will bypass validation and copy features."),
          highlight = Some("json"),
          handler = handleInputValue(text => ref => Vector(SetJsonString(ref, text.noSpaces))),
          collapsable = Some(false)))
      ).flatten

      val propertiesMap = elementProperties.elementSchema.properties.getOrElse(Map.empty)

      val schemaProperties = if (propertiesMap.nonEmpty || elementProperties.elementSchema.`type`.contains(Right("object"))) {
        elementProperties.elementSchema.properties.getOrElse(Map.empty).flatMap {
          case (propertyKey, propertySchema) => propertySpec(Some(propertyKey), propertySchema)
        }
      } else List(propertySpec(None, elementProperties.elementSchema)).flatten

      setProperties(commonProperties ++ schemaProperties, elementProperties)
    }

  def setProperties(properties: List[PropertySpec], elementProperties: ElementProperties) : IO[Unit] = for {
    newForm <- IO(EditorPropertiesHtml.createPropertyForm(properties))
    _ <- IO {

      if (EditorPropertiesHtml.propertiesBody.children.length == 0) {
        EditorPropertiesHtml.propertiesBody.appendChild(newForm.html)
      } else {
        EditorPropertiesHtml.propertiesBody.replaceChild(newForm.html, EditorPropertiesHtml.propertiesBody.firstChild)
      }

      newForm.groups.foreach { group =>
        group.init()
      }
    }
    _ <- newForm.groups.map(setGroupValues(_, elementProperties)).sequence
    _ <- currentProperties.set(newForm.groups)
  } yield ()

  def textRows(string: Option[String]): Int = string.map(_.split("\n").length).getOrElse(1)

  def propertySpec(key: Option[String], schema: EditorModel.EditorSchema): Option[PropertySpec] = schema.`type` match {
    case Some(Right(single)) => single match {
      case "string" => Some(inputProperty(key, TextInput, schema))
      case "integer" => Some(inputProperty(key, IntegerInput, schema))
      case "boolean" => Some(inputProperty(key, BooleanInput, schema))
      case "number" => Some(inputProperty(key, NumberInput, schema))
      case _ => None
    }
    case Some(Left(multiple)) => None
    case None => None
  }

  def inputProperty(key: Option[String], inputType: PropertyInputType, schema: EditorModel.EditorSchema): PropertySpec = {
    val propertyHandler = handleInputValue(newValue => ref => {
      val valueUpdate: Json => Json = key match {
        case Some(keyValue) => _.mapObject(_.add(keyValue, newValue))
        case None => _ => newValue
      }

      val additionalUpdates = if(schema.extension.exists(_.copyToLabel.contains(true))) Vector(
        SetLabel(ref, newValue.asString.getOrElse(newValue.noSpaces))
      ) else Vector.empty

      Vector(SetJson(ref, valueUpdate)) ++ additionalUpdates
    })

    PropertySpec(
      key = key,
      title = schema.title.orElse(key).getOrElse(""),
      description = schema.description,
      inputType = inputType,
      highlight = schema.extension.flatMap(_.highlight),
      handler = propertyHandler)
  }

  def getElementSchema(schemaRef: String, definitions: Map[String, EditorModel.EditorSchema]): Option[EditorModel.EditorSchema] =
    for {
      definitionKey <- Try(schemaRef.stripPrefix("#/$defs/")).toOption
      schemaAtKey <- definitions.get(definitionKey)
    } yield schemaAtKey

  def getElementProperties(elementRef: ElementRef,
                           graphml: GraphMLGraph[Json, Json],
                           schema: EditorModel.EditorSchema): Option[ElementProperties] = for {
    element <- elementRef match {
      case ElementRef(id, NodeType) => graphml.graph.findNode(id).map[GraphMLElement[Json]](_.value)
      case ElementRef(id, EdgeType) => graphml.graph.findEdge(id).map[GraphMLElement[Json]](_.value)
    }
    definitions = schema.definitionsCompat getOrElse Map.empty
    elementSchema <- element.schemaRef.flatMap(getElementSchema(_, definitions)).orElse(Some(Schema[EditorSchemaHints]()))
  } yield ElementProperties(elementRef, element.label.map(_.text), elementSchema, element.value)

}
