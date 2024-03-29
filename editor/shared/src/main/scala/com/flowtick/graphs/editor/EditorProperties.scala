package com.flowtick.graphs.editor

import cats.effect.IO
import cats.implicits._
import com.flowtick.graphs.json.schema.Schema
import io.circe.Json

import scala.util.Try
import cats.effect.kernel.Ref
import com.flowtick.graphs.view.{EdgeElementType, ElementRef, ElementType, NodeElementType}

import scala.concurrent.Future

final case class EditorPropertiesValues(
    element: ElementRef,
    labelValue: Option[String],
    colorValue: Option[String],
    elementSchema: EditorModel.EditorSchema,
    value: Json
)

sealed trait PropertyInputType

case object LabelInputType extends PropertyInputType
case object ColorInputType extends PropertyInputType
case object JsonInputType extends PropertyInputType

case object TextInput extends PropertyInputType {
  def fromJson(key: Option[PropertyValueKey], json: Json): Option[String] =
    key
      .flatMap(_.name)
      .map(keyValue =>
        json.hcursor
          .downField(keyValue)
          .focus
          .flatMap(_.asString)
      )
      .getOrElse(json.asString)
}

case object IntegerInput extends PropertyInputType {
  def fromJson(key: Option[PropertyValueKey], json: Json): Option[Int] = {
    key
      .flatMap(_.name)
      .map(keyValue =>
        json.hcursor
          .downField(keyValue)
          .focus
          .flatMap(_.asNumber)
      )
      .getOrElse(json.asNumber)
      .flatMap(_.toInt)
  }
}

case object BooleanInput extends PropertyInputType {
  def fromJson(key: Option[PropertyValueKey], json: Json): Option[Boolean] = {
    key
      .flatMap(_.name)
      .map(keyValue =>
        json.hcursor
          .downField(keyValue)
          .focus
          .flatMap(_.asBoolean)
      )
      .getOrElse(json.asBoolean)
  }
}
case object NumberInput extends PropertyInputType {
  def fromJson(key: Option[PropertyValueKey], json: Json): Option[Double] = {
    key
      .flatMap(_.name)
      .map(keyValue =>
        json.hcursor
          .downField(keyValue)
          .focus
          .flatMap(_.asNumber)
      )
      .getOrElse(json.asNumber)
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

final case class PropertySpec(
    key: Option[PropertyValueKey] = None, // no key means its the root value
    title: String,
    inputType: PropertyInputType,
    handler: PropertySpec.Handler,
    description: Option[String] = None,
    collapsable: Option[Boolean] = None,
    highlight: Option[String] = None,
    order: Float = 0.5f,
    targetType: Option[ElementType] = None
)

object PropertySpec {
  type Handler = PropertyValue => IO[Unit]
}

trait PropertyControl {
  def property: PropertySpec
  def init: IO[Unit]

  /** set the value of the control
    *
    * note: this must not call the properly handler, which would create a loop
    *
    * @return
    */
  def set: Json => IO[Unit]
}

trait EditorProperties extends EditorComponent {
  def messageBus: EditorMessageBus
  def initEditor(editorModel: EditorModel): IO[Unit]
  def toggleEdit(enabled: Boolean): IO[Boolean]

  protected def createPropertyControls(
      properties: List[PropertySpec],
      values: EditorPropertiesValues
  ): IO[List[PropertyControl]]

  override def order: Double = 0.3

  lazy val currentElement: Ref[IO, Option[ElementRef]] = Ref.unsafe(None)
  lazy val currentControls: Ref[IO, List[PropertyControl]] =
    Ref.unsafe(List.empty)

  override def init(model: EditorModel): IO[Unit] = for {
    _ <- initEditor(model)
  } yield ()

  override lazy val eval: Eval = ctx =>
    ctx.effect(this) {
      case Selected(elements, _) =>
        elements.headOption
          .flatMap(getPropertiesValues(_, ctx.model))
          .map(setElementAndReset)
          .getOrElse(IO.unit)

      case ElementUpdated(updatedElement, _, _) =>
        for {
          current <- currentElement.get
          _ <-
            if (current.contains(updatedElement)) {
              current
                .flatMap(getPropertiesValues(_, ctx.model))
                .map(updateValues)
                .getOrElse(IO.unit)
            } else IO.unit
        } yield ()

      case EditorToggle(EditorToggle.editKey, value) =>
        for {
          _ <- toggleEdit(value.getOrElse(true))
        } yield ()
    }

  private def setElementAndReset(
      values: EditorPropertiesValues
  ): IO[Unit] = {
    for {
      _ <- currentElement.set(Some(values.element))
      _ <- resetProperties(values)
    } yield ()
  }

  def resetProperties(values: EditorPropertiesValues): IO[Unit] = {
    val commonProperties: List[PropertySpec] = List(
      Some(
        PropertySpec(
          title = "Label",
          order = 0.0f,
          inputType = LabelInputType,
          handler = handlePropertyValue { case JsonValue(json) =>
            ref => Vector(SetLabel(ref, json.asString.getOrElse("")))
          }
        )
      ).filterNot(_ =>
        values.elementSchema.extension.exists(
          _.hideLabelProperty.contains(true)
        )
      ),
      Some(
        PropertySpec(
          title = "Color",
          order = 0.1f,
          inputType = ColorInputType,
          handler = handlePropertyValue { case ColorValue(color) =>
            ref => Vector(SetColor(ref, color))
          }
        )
      ).filterNot(_ =>
        values.elementSchema.extension.exists(
          _.hideLabelProperty.contains(true)
        )
      ),
      values.elementSchema.extension
        .filter(_.showJsonProperty.contains(true))
        .flatMap(_ =>
          Some(
            PropertySpec(
              title = "Json",
              order = 1.0f,
              inputType = JsonInputType,
              description = Some(
                "Set the JSON value directly, note: this will bypass validation and copy features."
              ),
              highlight = Some("json"),
              handler = handlePropertyValue { case JsonValue(json) =>
                ref => Vector(SetJsonString(ref, json.noSpaces))
              },
              collapsable = Some(false)
            )
          )
        )
    ).flatten

    val propertiesMap =
      values.elementSchema.properties.getOrElse(Map.empty)

    val schemaProperties =
      if (
        propertiesMap.nonEmpty || values.elementSchema.`type`
          .contains(Right("object"))
      ) {
        values.elementSchema.properties
          .getOrElse(Map.empty)
          .flatMap { case (propertyKey, propertySchema) =>
            propertySpec(Some(propertyKey), propertySchema)
          }
      } else List(propertySpec(None, values.elementSchema)).flatten

    setProperties(commonProperties ++ schemaProperties, values)
  }

  def handlePropertyValue(
      f: PartialFunction[PropertyValue, ElementRef => Vector[EditorEvent]]
  ): PropertyValue => IO[Unit] = (value: PropertyValue) =>
    for {
      elem <- currentElement.get
      _ <- elem match {
        case Some(ref) =>
          if (f.isDefinedAt(value)) {
            IO(f(value)(ref)).flatMap { events =>
              events.map {
                case command: EditorCommand => messageBus.publish(command)
                case other                  => messageBus.notifyEvent(this, other)
              }.sequence
            }
          } else IO(println(s"unhandled value $value"))
        case None => IO.unit
      }
    } yield ()

  def setProperties(
      properties: List[PropertySpec],
      elementProperties: EditorPropertiesValues
  ): IO[Unit] = for {
    newGroups <- createPropertyControls(properties, elementProperties)
    _ <- newGroups.map(updatePropertyControl(_, elementProperties)).sequence
    _ <- newGroups.map(_.init).sequence
    _ <- currentControls.set(newGroups)
  } yield ()

  protected def updatePropertyControl(
      control: PropertyControl,
      elementProperties: EditorPropertiesValues
  ): IO[PropertyControl] =
    (control.property.inputType match {
      case LabelInputType =>
        control.set(Json.fromString(elementProperties.labelValue.getOrElse("")))
      case ColorInputType =>
        control.set(
          Json.fromString(elementProperties.colorValue.getOrElse("#FFFFFF"))
        )
      case _ => control.set(elementProperties.value)
    }) *> IO.pure(control)

  private def updateValues(values: EditorPropertiesValues): IO[Unit] =
    for {
      properties <- currentControls.get
      _ <- properties.map(updatePropertyControl(_, values)).sequence
    } yield ()

  def getPropertiesValues(
      elementRef: ElementRef,
      model: EditorModel
  ): Option[EditorPropertiesValues] = for {
    element <- elementRef match {
      case ElementRef(id, NodeElementType) =>
        model.graph.findNode(id).map[EditorGraphElement](_.value)
      case ElementRef(id, EdgeElementType) =>
        model.graph.findEdge(id).map[EditorGraphElement](_.value)
    }

    color = elementRef match {
      case ElementRef(id, NodeElementType) =>
        model.styleSheet
          .requireNodeStyle(Some(id), List.empty)
          .fill
          .flatMap(_.color)
      case ElementRef(id, EdgeElementType) =>
        model.styleSheet
          .requireEdgeStyle(Some(id), List.empty)
          .edgeStyle
          .map(_.color)
    }

    elementSchema <- element.schemaRef
      .flatMap(getElementSchema(_, model.schema.definitions))
      .orElse(Some(Schema[EditorSchemaHints]()))
  } yield EditorPropertiesValues(
    elementRef,
    element.label,
    color,
    elementSchema,
    element.data
  )

  def getElementSchema(
      schemaRef: String,
      definitions: Map[String, EditorModel.EditorSchema]
  ): Option[EditorModel.EditorSchema] =
    for {
      definitionKey <- Try(schemaRef.stripPrefix("#/$defs/")).toOption
      schemaAtKey <- definitions.get(definitionKey)
    } yield schemaAtKey

  def propertySpec(
      key: Option[String],
      schema: EditorModel.EditorSchema
  ): Option[PropertySpec] = schema.`type` match {
    case Some(Right(single)) =>
      single match {
        case "string"  => Some(inputProperty(key, TextInput, schema))
        case "integer" => Some(inputProperty(key, IntegerInput, schema))
        case "boolean" => Some(inputProperty(key, BooleanInput, schema))
        case "number"  => Some(inputProperty(key, NumberInput, schema))
        case _         => None
      }
    case Some(Left(_)) => None // multiple not supported yet
    case None          => None
  }

  def inputProperty(
      key: Option[String],
      inputType: PropertyInputType,
      schema: EditorModel.EditorSchema
  ): PropertySpec = {
    val propertyHandler = handlePropertyValue { case JsonValue(newValue) =>
      ref => {
        val valueUpdate: Json => Json = key match {
          case Some(keyValue) => _.mapObject(_.add(keyValue, newValue))
          case None           => _ => newValue
        }

        val additionalUpdates =
          if (schema.extension.exists(_.copyToLabel.contains(true)))
            Vector(
              SetLabel(ref, newValue.asString.getOrElse(newValue.noSpaces))
            )
          else Vector.empty

        Vector(SetJson(ref, valueUpdate)) ++ additionalUpdates
      }
    }

    PropertySpec(
      key = Some(JsonValueKey(key)),
      title = schema.title.orElse(key).getOrElse(""),
      description = schema.description,
      inputType = inputType,
      highlight = schema.extension.flatMap(_.highlight),
      handler = propertyHandler
    )
  }
}

object EditorProperties {
  import cats.effect.unsafe.implicits.global

  def eventHandler[E](f: E => IO[Any]): E => Future[Any] =
    (event: E) => f(event).unsafeToFuture()
}
