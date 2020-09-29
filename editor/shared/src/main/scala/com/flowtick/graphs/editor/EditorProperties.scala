package com.flowtick.graphs.editor

import cats.implicits._
import cats.effect.IO
import cats.effect.concurrent.Ref
import com.flowtick.graphs.graphml.{GraphMLElement, GraphMLGraph}
import com.flowtick.graphs.json.schema.Schema
import io.circe.Json

import scala.util.Try

final case class ElementProperties(element: ElementRef,
                                   labelValue: Option[String],
                                   colorValue: Option[String],
                                   elementSchema: EditorModel.EditorSchema,
                                   value: Json)

sealed trait PropertyInputType

case object LabelInputType extends PropertyInputType
case object ColorInputType extends PropertyInputType
case object JsonInputType extends PropertyInputType

case object TextInput extends PropertyInputType {
  def fromJson(key: Option[PropertyValueKey], json: Json): Option[String] = {
    key.flatMap(_.name).map(keyValue => json.hcursor.downField(keyValue)
      .focus
      .flatMap(_.asString)).getOrElse(json.asString)
  }
}

case object IntegerInput extends PropertyInputType {
  def fromJson(key: Option[PropertyValueKey], json: Json): Option[Int] = {
    key.flatMap(_.name).map(keyValue => json.hcursor.downField(keyValue)
      .focus
      .flatMap(_.asNumber)).getOrElse(json.asNumber).flatMap(_.toInt)
  }
}

case object BooleanInput extends PropertyInputType {
  def fromJson(key: Option[PropertyValueKey], json: Json): Option[Boolean] = {
    key.flatMap(_.name).map(keyValue => json.hcursor.downField(keyValue)
      .focus
      .flatMap(_.asBoolean)).getOrElse(json.asBoolean)
  }
}
case object NumberInput extends PropertyInputType {
  def fromJson(key: Option[PropertyValueKey], json: Json): Option[Double] = {
    key.flatMap(_.name).map(keyValue => json.hcursor.downField(keyValue)
      .focus
      .flatMap(_.asNumber)).getOrElse(json.asNumber)
      .map(_.toDouble)
  }
}

sealed trait PropertyValue
case class JsonValue(json: Json) extends PropertyValue
case class ColorValue(color: String) extends PropertyValue

sealed trait PropertyValueKey {
  def name: Option[String]
}

case class JsonValueKey(key: Option[String]) extends PropertyValueKey {
  override def name: Option[String] = key
}

case object ColorKey extends PropertyValueKey {
  override def name: Option[String] = None
}

final case class PropertySpec(key: Option[PropertyValueKey] = None, // no key means its the root value
                              title: String,
                              inputType: PropertyInputType,
                              handler: PropertyValue => Unit,
                              description: Option[String] = None,
                              collapsable: Option[Boolean] = None,
                              highlight: Option[String] = None,
                              order: Float = 0.5f,
                              targetType: Option[ElementType] = None)

trait PropertyFormGroup {
  def property: PropertySpec
  def init: () => Unit
  def set: Json => Unit
}


trait EditorProperties extends EditorComponent {
  def messageBus: EditorMessageBus
  def initEditor(editorModel: EditorModel): IO[Unit]
  def toggleEdit(enabled: Boolean): IO[Boolean]

  protected def setPropertiesGroups(properties: List[PropertySpec], elementProperties: ElementProperties): IO[List[PropertyFormGroup]]

  override def order: Double = 0.3

  lazy val currentElement: Ref[IO, Option[ElementRef]] = Ref.unsafe(None)
  lazy val currentProperties: Ref[IO, List[PropertyFormGroup]] = Ref.unsafe(List.empty)

  override def init(model: EditorModel): IO[Unit] = for {
    _ <- initEditor(model)
  } yield ()


  override lazy val eval: Eval = ctx => ctx.effect(this) {
    case Selected(elements, _) =>
      elements
        .headOption
        .flatMap(getElementProperties(_, ctx.model.graphml, ctx.model.schema))
        .map(setElementAndReset)
        .getOrElse(IO.unit)

    case ElementUpdated(updatedElement, _, _) =>
      for {
        current <- currentElement.get
        _ <- if (current.contains(updatedElement)) {
          current
            .flatMap(getElementProperties(_, ctx.model.graphml, ctx.model.schema))
            .map(updateValues)
            .getOrElse(IO.unit)
        } else IO.unit
      } yield ()

    case Toggle(Toggle.editKey, value) => for {
      _ <- toggleEdit(value.getOrElse(true))
    } yield ()
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
        handler = handleInputValue {
          case JsonValue(json) => ref => Vector(SetLabel(ref, json.asString.getOrElse("")))
        }
      )).filterNot(_ => elementProperties.elementSchema.extension.exists(_.hideLabelProperty.contains(true))),

      Some(PropertySpec(
        title = "Color",
        order = 0.1f,
        inputType = ColorInputType,
        handler = handleInputValue {
          case ColorValue(color) => ref => Vector(SetColor(ref, color))
        }
      )).filterNot(_ => elementProperties.elementSchema.extension.exists(_.hideLabelProperty.contains(true))),

      elementProperties.elementSchema.`extension`.filter(_.showJsonProperty.contains(true)).flatMap(_ => Some(PropertySpec(
        title = "Json",
        order = 1.0f,
        inputType = JsonInputType,
        description = Some("Set the JSON value directly, note: this will bypass validation and copy features."),
        highlight = Some("json"),
        handler = handleInputValue {
          case JsonValue(json) => ref => Vector(SetJsonString(ref, json.noSpaces))
        },
        collapsable = Some(false)))
    )).flatten

    val propertiesMap = elementProperties.elementSchema.properties.getOrElse(Map.empty)

    val schemaProperties = if (propertiesMap.nonEmpty || elementProperties.elementSchema.`type`.contains(Right("object"))) {
      elementProperties.elementSchema.properties.getOrElse(Map.empty).flatMap {
        case (propertyKey, propertySchema) => propertySpec(Some(propertyKey), propertySchema)
      }
    } else List(propertySpec(None, elementProperties.elementSchema)).flatten

    setProperties(commonProperties ++ schemaProperties, elementProperties)
  }

  def handleInputValue(f: PropertyValue => ElementRef => Vector[EditorEvent]): PropertyValue => Unit = (value: PropertyValue) => (for {
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

  def setProperties(properties: List[PropertySpec], elementProperties: ElementProperties) : IO[Unit] = for {
    newGroups <- setPropertiesGroups(properties, elementProperties)
    _ <- newGroups.map(setGroupValues(_, elementProperties)).sequence
    _ <- IO(newGroups.foreach { group =>
      group.init()
    })
    _ <- currentProperties.set(newGroups)
  } yield ()

  protected def setGroupValues(group: PropertyFormGroup, elementProperties: ElementProperties): IO[PropertyFormGroup] = IO {
    group.property.inputType match {
      case LabelInputType => group.set(Json.fromString(elementProperties.labelValue.getOrElse("")))
      case ColorInputType => group.set(Json.fromString(elementProperties.colorValue.getOrElse("#FFFFFF")))
      case _ => group.set(elementProperties.value)
    }
    group
  }

  private def updateValues(elementProperties: ElementProperties): IO[Unit] = for {
    properties <- currentProperties.get
    _ <- properties.map(setGroupValues(_, elementProperties)).sequence
  } yield ()

  def getElementProperties(elementRef: ElementRef,
                           graphml: GraphMLGraph[Json, Json],
                           schema: EditorModel.EditorSchema): Option[ElementProperties] = for {
    element <- elementRef match {
      case ElementRef(id, NodeType) => graphml.graph.findNode(id).map[GraphMLElement[Json]](_.value)
      case ElementRef(id, EdgeType) => graphml.graph.findEdge(id).map[GraphMLElement[Json]](_.value)
    }
    definitions = schema.definitionsCompat getOrElse Map.empty
    elementSchema <- element.schemaRef.flatMap(getElementSchema(_, definitions)).orElse(Some(Schema[EditorSchemaHints]()))
  } yield ElementProperties(elementRef, element.label.map(_.text), element.fill.flatMap(_.color), elementSchema, element.value)

  def getElementSchema(schemaRef: String, definitions: Map[String, EditorModel.EditorSchema]): Option[EditorModel.EditorSchema] =
    for {
      definitionKey <- Try(schemaRef.stripPrefix("#/$defs/")).toOption
      schemaAtKey <- definitions.get(definitionKey)
    } yield schemaAtKey

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
    val propertyHandler = handleInputValue {
      case JsonValue(newValue) => ref => {
        val valueUpdate: Json => Json = key match {
          case Some(keyValue) => _.mapObject(_.add(keyValue, newValue))
          case None => _ => newValue
        }

        val additionalUpdates = if(schema.extension.exists(_.copyToLabel.contains(true))) Vector(
          SetLabel(ref, newValue.asString.getOrElse(newValue.noSpaces))
        ) else Vector.empty

        Vector(SetJson(ref, valueUpdate)) ++ additionalUpdates
      }
    }

    PropertySpec(
      key = Some(JsonValueKey(key)),
      title = schema.title.orElse(key).getOrElse(""),
      description = schema.description,
      inputType = inputType,
      highlight = schema.extension.flatMap(_.highlight),
      handler = propertyHandler)
  }

}
