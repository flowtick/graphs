package com.flowtick.graphs.editor

import java.util.UUID

import cats.data.Validated.{Valid, _}
import cats.data.ValidatedNel
import cats.effect.IO
import com.flowtick.graphs.graphml.{Datatype, DatatypeString, FromGraphML, GraphMLEdge, GraphMLGraph, GraphMLKey, GraphMLMeta, GraphMLNode, GraphMLResource, Image, NodeShape, ToGraphML, graphMLDataType}
import com.flowtick.graphs.json.schema.{JsonSchema, Schema}
import com.flowtick.graphs.json.{FromJson, ToJson}
import com.flowtick.graphs.layout.DefaultGeometry
import com.flowtick.graphs.{Edge, Node}
import io.circe.{Json, _}

import scala.xml.NodeSeq

class EditorModelUpdate extends EditorComponent {

  override def order: Double = 0.1

  implicit val jsonDataType = EditorModel.jsonDataType(true)

  val handleAddNode: Transform = ctx => ctx.transform {
    case add: CreateNode =>
      val allItems = ctx.model.palette.stencils.flatMap(_.items)

      add
        .stencilRef
        .flatMap(id => allItems.find(_.id == id))
        .orElse(allItems.headOption)
        .flatMap { item =>
          for {
            xPos <- add.x.orElse(Some(0.0))
            yPos <- add.y.orElse(Some(0.0))
            geometry = Some(DefaultGeometry(
              xPos,
              yPos,
              item.shape.flatMap(_.geometry.map(_.width)).getOrElse(80.0),
              item.shape.flatMap(_.geometry.map(_.height)).getOrElse(50.0))
            )

            shape = item.shape
              .map(_.copy(geometry = geometry))
              .getOrElse(NodeShape(geometry = geometry))
          } yield item match {
            case Stencil(_, _, _, None, _, schemaRef) =>
              withNewNode(
                ctx,
                Node.of(GraphMLNode(add.id, Json.obj(), shape = Some(shape), schemaRef = schemaRef))
              )

            case Stencil(id, _, _, Some(imageDataRef), _, schemaRef) =>
              ctx.model.graphml.meta.resources.find(_.id == id) match {
                case Some(imageFound) =>
                  val imageShape = Some(shape.copy(image = Some(Image(imageFound.id))))
                  withNewNode(
                    ctx,
                    Node.of(GraphMLNode(add.id, Json.obj(), shape = imageShape, schemaRef = schemaRef))
                  )
                case None =>
                  val withImage = for {
                    image <- ctx.model.palette.images.get(imageDataRef)
                  } yield {
                    val newResource = GraphMLResource(id, image.data, Some("dataUrl"))
                    val urlImageShape = shape.copy(image = Some(Image(newResource.id)))

                    withNewNode(
                      ctx.updateModel(_.updateGraphMl(ctx.model.graphml.addResource(newResource))),
                      Node.of(GraphMLNode(add.id, Json.obj(), shape = Some(urlImageShape), schemaRef = schemaRef))
                    )
                  }

                  withImage.getOrElse(ctx)
              }
            case _ => ctx
          }
      }.getOrElse(ctx)

      // TODO: handle refs to images and add them to graph resources
  }

  private def withNewNode(ctx: EditorContext, newNode: Node[GraphMLNode[Json]]): EditorContext = {
    val updatedGraph = ctx.model.graphml.copy(graph = ctx.model.graphml.graph withNode newNode)
    ctx
      .copy(model = ctx.model.updateGraphMl(updatedGraph))
      .addNotification(this, ElementUpdated(ElementRef(newNode.id, NodeType), Created))
  }

  val handleAddEdge: Transform = ctx => ctx.transform {
    case edge: AddEdge => addEdge(edge)(ctx)
  }

  def addEdge(edge: AddEdge)(ctx: EditorContext): EditorContext = {
    val allItems = ctx.model.palette.connectors.flatMap(_.items)

    val connector: Option[Connector] = edge
      .stencilRef
      .flatMap(id => allItems.find(_.id == id))
      .orElse(allItems.headOption)

    val mlEdge = GraphMLEdge(
      edge.id,
      Json.obj(),
      Some(edge.from),
      Some(edge.to),
      schemaRef = connector.flatMap(_.schemaRef),
      shape = connector.flatMap(_.shape.map(_.copy(path = edge.path)))
    )

    (for {
      fromNode <- ctx.model.graphml.graph.findNode(edge.from)
      toNode <- ctx.model.graphml.graph.findNode(edge.to)
      newEdge = Edge(edge.id, mlEdge, fromNode.id, toNode.id)
    } yield {
      val updated = ElementUpdated(ElementRef(newEdge.id, EdgeType), Created)
      val graphMl = ctx.model.updateGraphMl(ctx.model.graphml.copy(graph = ctx.model.graphml.graph withEdge newEdge))
      ctx
        .copy(model = graphMl)
        .addNotification(this, updated)
    }).getOrElse(ctx)
  }

  val load: Transform = ctx => ctx.transform {
    case Load(xml, GraphMLFormat) => FromGraphML[Json, Json](xml) match {
      case Right(graphml) => ctx.copy(
        model = ctx.model
          .updateGraphMl(graphml)
          .withSchema(getSchemaFromGraph(graphml).getOrElse(ctx.model.schema))
      ).addNotification(this, SetGraph(graphml))
      case Left(errors) => ctx.addError(this, errors.last)
    }

    case Load(json, JsonFormat) =>
      import com.flowtick.graphs.json.format.default._
      import io.circe.generic.auto._

      FromJson[GraphMLMeta, GraphMLEdge[Json], GraphMLNode[Json]](json) match {
        case Right(jsonGraph) =>
          val graphMLGraph = GraphMLGraph(jsonGraph.graph, jsonGraph.meta.getOrElse(GraphMLMeta()))
          ctx.copy(
            model = ctx.model
              .updateGraphMl(graphMLGraph)
              .withSchema(getSchemaFromGraph(graphMLGraph).getOrElse(ctx.model.schema))
          ).addNotification(this, SetGraph(graphMLGraph))
        case Left(error) => ctx.addError(this, new RuntimeException(error))
      }
  }

  val setModel: Transform = ctx => ctx.transform {
    case SetModel(model) => ctx.copy(model = model)
  }

  val moveNode: Transform = ctx => ctx.transform {
    case move@MoveTo(ElementRef(id, NodeType), x, y) =>
      val updatedGraph = ctx.model.graphml.graph.updateNode(id)(_.updateNodeGeometry(_ => x, _ => y))

      ctx
        .copy(model = ctx.model.updateGraphMl(ctx.model.graphml.copy(graph = updatedGraph)))
        .addNotification(this, ElementUpdated(ElementRef(id, NodeType), Changed, causedBy = Some(move)))

    case move@MoveBy(deltaX, deltaY) =>
      ctx.model.selection.foldLeft(ctx) {
        case (updatedCtx, ElementRef(id, NodeType)) =>
          val updatedGraph = updatedCtx.model.graphml.graph.updateNode(id)(node => node.updateNodeGeometry(_ + deltaX, _ + deltaY))

          updatedCtx
            .copy(model = updatedCtx.model.updateGraphMl(updatedCtx.model.graphml.copy(graph = updatedGraph)))
            .addNotification(this, ElementUpdated(ElementRef(id, NodeType), Changed, causedBy = Some(move)))

        case (updatedCtx, _) => updatedCtx
      }
  }

  val setNodeLabel: Transform = ctx => ctx.transform {
    case SetLabel(ElementRef(id, EdgeType), text) =>
      val updatedGraph = ctx.model.graphml.graph.updateEdge(id)(_.updateEdgeLabel(text))

      ctx
        .copy(model = ctx.model.updateGraphMl(ctx.model.graphml.copy(graph = updatedGraph)))
        .addNotification(this, ElementUpdated(ElementRef(id, EdgeType)))

    case SetLabel(ElementRef(id, NodeType), text) =>
      val updatedGraph = ctx.model.graphml.graph.updateNode(id)(_.updateNodeLabel(text))

      ctx
        .copy(model = ctx.model.updateGraphMl(ctx.model.graphml.copy(graph = updatedGraph)))
        .addNotification(this, ElementUpdated(ElementRef(id, NodeType)))
  }

  val setNodeJson: Transform = ctx => ctx.transform {
    case SetJson(ElementRef(id, NodeType), json) =>
      updateNodeJson(ctx, id, json)

    case SetJson(ElementRef(id, EdgeType), json) =>
      updateEdgeJson(ctx, id, json)

    case SetJsonString(ElementRef(id, NodeType), json) =>
      io.circe.parser.decode[Json](json) match {
        case Right(json) =>
          updateNodeJson(ctx, id, _ => json)

        case Left(error) => ctx.addError(this, error)
      }

    case SetJsonString(ElementRef(id, EdgeType), json) =>
      io.circe.parser.decode[Json](json) match {
        case Right(json) =>
          updateEdgeJson(ctx, id, _ => json)

        case Left(error) => ctx.addError(this, error)
      }
  }

  private def updateNodeJson(ctx: EditorContext, id: String, update: Json => Json): EditorContext = {
    val updatedGraph = ctx.model.graphml.graph.updateNode(id)(_.updateValue(update))

    ctx
      .copy(model = ctx.model.updateGraphMl(ctx.model.graphml.copy(graph = updatedGraph)))
      .addNotification(this, ElementUpdated(ElementRef(id, NodeType)))
  }

  private def updateEdgeJson(ctx: EditorContext, id: String, json: Json => Json): EditorContext = {
    val updatedGraph = ctx.model.graphml.graph.updateEdge(id)(_.updateValue(json))

    ctx
      .copy(model = ctx.model.updateGraphMl(ctx.model.graphml.copy(graph = updatedGraph)))
      .addNotification(this, ElementUpdated(ElementRef(id, EdgeType)))
  }

  val handleExport: Transform = ctx => ctx.transform {
    case Export(GraphMLFormat) =>
      ctx.addNotification(this, ExportedGraph(ctx.model.graphml.meta.id.getOrElse("graph"), ToGraphML[Json, Json](
        graphWithSchema(ctx.model.graphml, ctx.model.schema)
      ).toString, GraphMLFormat))
    case Export(JsonFormat) =>
      import com.flowtick.graphs.json.format.default._
      import io.circe.generic.auto._

      ctx.addNotification(this, ExportedGraph(
        ctx.model.graphml.meta.id.getOrElse("graph"),
        ToJson[GraphMLMeta, GraphMLEdge[Json], GraphMLNode[Json]](
          ctx.model.graphml.graph, Some(graphWithSchema(ctx.model.graphml, ctx.model.schema).meta)
        ).spaces2,
        JsonFormat
      ))
  }

  def graphWithSchema(graph: GraphMLGraph[Json, Json], schema: EditorModel.EditorSchema): GraphMLGraph[Json, Json] = {
    import io.circe.generic.auto._

    val schemaResource = GraphMLResource(
      "schema",
      Printer.noSpaces.copy(dropNullValues = true).print(JsonSchema.toJson[EditorSchemaHints](schema)),
      Some("schema")
    )
    graph.addResource(schemaResource)
  }

  def getSchemaFromGraph(graph: GraphMLGraph[Json, Json]): Option[EditorModel.EditorSchema] = {
    import io.circe.generic.auto._

    for {
      schemaResource <- graph.meta.resources.find(_.typeHint.contains("schema"))
      schema <- JsonSchema.parse[EditorSchemaHints](schemaResource.value).toOption
    } yield schema
  }

  val handleSelect: Transform = ctx => ctx.transform {
    case SelectAll =>
      val allNodes = ctx.model.graphml.graph.nodes.map(node => ElementRef(node.id, NodeType)).toSet
      withSelection(ctx, allNodes, append = false)

    case Select(selection, append) =>
      withSelection(ctx, selection, append)
  }

  private def withSelection(ctx: EditorContext, selection: Set[ElementRef], append: Boolean) = {
    val oldSelection = ctx.model.selection

    val newSelection = selection.flatMap {
      case ElementRef(id, NodeType) =>
        ctx.model.graphml.graph.findNode(id).map(node => ElementRef(node.id, NodeType))
      case ElementRef(id, EdgeType) =>
        ctx.model.graphml.graph.findEdge(id).map(edge => ElementRef(edge.id, EdgeType))
    } ++ (if (append) oldSelection else List.empty)

    val withConnected: Option[EditorContext] = for {
      from <- oldSelection.headOption
      if ctx.model.connectSelection
      to <- newSelection.headOption
      if from.elementType == NodeType && to.elementType == NodeType
    } yield {
      ctx
        .addCommand(AddEdge(UUID.randomUUID().toString, from.id, to.id))
        .updateModel(_.copy(connectSelection = false))
    }

    withConnected
      .getOrElse(ctx)
      .updateModel(_.copy(selection = newSelection))
      .addNotification(this, Selected(newSelection, oldSelection))
  }

  val handleConnectToggle: Transform = ctx => ctx.transform {
    case Toggle(Toggle.connectKey, toggle) => ctx.copy(model = ctx.model.copy(connectSelection = toggle))
  }

  val handleDelete: Transform = ctx => ctx.transform {
    case DeleteSelection =>
      ctx.model.selection.foldLeft(ctx) {
        case (elementCtx, ref) if ref.elementType == NodeType =>
          val edges = elementCtx.model.graphml.graph.incoming(ref.id) ++ elementCtx.model.graphml.graph.outgoing(ref.id)

          val withoutEdges = edges.foldLeft(elementCtx) {
            case (edgeContext, edge) =>
              edgeContext
                .updateModel(model => model.copy(graphml = model.graphml.removeEdge(edge.id)))
                .addNotification(this, ElementUpdated(ElementRef(edge.id, EdgeType), Deleted))
          }

          withoutEdges
            .updateModel(model => model.copy(graphml = model.graphml.removeNode(ref.id)))
            .addNotification(this, ElementUpdated(ElementRef(ref.id, NodeType), Deleted))

        case (context, ref) if ref.elementType == EdgeType =>
          context
            .updateModel(model => model.copy(graphml = model.graphml.removeEdge(ref.id)))
            .addNotification(this, ElementUpdated(ElementRef(ref.id, EdgeType), Deleted))
      }
  }

  lazy val handleAll: Transform = handleAddEdge
    .andThen(handleAddNode)
    .andThen(setNodeLabel)
    .andThen(setNodeJson)
    .andThen(moveNode)
    .andThen(load)
    .andThen(setModel)
    .andThen(handleExport)
    .andThen(handleSelect)
    .andThen(handleConnectToggle)
    .andThen(handleDelete)

  override lazy val eval: Eval = ctx => IO.pure(handleAll(ctx))
}

final case class EditorModel(graphml: GraphMLGraph[Json, Json],
                             schema: EditorModel.EditorSchema,
                             selection: Set[ElementRef] = Set.empty,
                             connectSelection: Boolean = false,
                             palette: Palette,
                             version: Long = 0) {
  def updateGraphMl(updated: GraphMLGraph[Json, Json]): EditorModel = copy(graphml = updated, version = version + 1)
  override def toString: String = {
    s"""version = $version, connect = $connectSelection, selection = $selection"""
  }

  def withSchema(schema: EditorModel.EditorSchema): EditorModel = copy(schema = schema)
}

final case class EditorSchemaHints(copyToLabel: Option[Boolean] = None,
                                   hideLabelProperty: Option[Boolean] = None,
                                   highlight: Option[String] = None)

object EditorModel {
  type EditorSchema = Schema[EditorSchemaHints]

  def jsonDataType(emptyOnError: Boolean)(implicit stringDatatype: Datatype[String]): Datatype[Json] = new Datatype[Json] {
    override def serialize(value: Json, targetHint: Option[String]): NodeSeq = stringDatatype.serialize(value.noSpaces, targetHint)
    override def deserialize(from: NodeSeq,
                             graphKeys: collection.Map[String, GraphMLKey],
                             targetHint: Option[String]): ValidatedNel[Throwable, Json] =
      stringDatatype
        .deserialize(from, graphKeys, targetHint)
        .map(io.circe.parser.decode[Json]) match {
        case Valid(Right(json)) => valid(json)
        case Valid(Left(error)) => if (emptyOnError) validNel(Json.obj()) else invalidNel(new IllegalArgumentException("could not parse json", error))
        case Invalid(errors) => if (emptyOnError) validNel(Json.obj()) else invalid(errors.prepend(new IllegalArgumentException("unable to parse xml node")))
      }

    override def keys(targetHint: Option[String]): Seq[GraphMLKey] = stringDatatype.keys(targetHint)
  }
}
