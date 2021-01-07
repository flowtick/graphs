package com.flowtick.graphs.editor.feature

import cats.effect.IO
import com.flowtick.graphs.editor.ImageLoader.unescapeXml
import com.flowtick.graphs.editor._
import com.flowtick.graphs.graphml.{Datatype, FromGraphML, GraphMLGraph, GraphMLResource}
import com.flowtick.graphs.json.schema.{JsonSchema, Schema}
import com.flowtick.graphs.layout.DefaultGeometry
import com.flowtick.graphs.style.{Fill, ImageSpec, NodeShape, StyleSheets}
import com.flowtick.graphs.{Edge, Node}
import io.circe.{Json, Printer}

import java.util.UUID

class ModelUpdateFeature extends EditorComponent {

  override def order: Double = 0.1

  implicit val jsonDataType: Datatype[Json] = EditorModel.jsonDataType(emptyOnError = true)

  val handleAddNode: Transform = ctx => ctx.transform {
    case addNode: AddNode =>
      val nodeToAdd = addNode
        .stencilRef
        .flatMap(ctx.model.palette.findStencil)
        .orElse(ctx.model.palette.stencils.headOption) match {
        case Some(Stencil(stencil, _, _, schemaRef)) =>
          Node(addNode.id, EditorGraphNode(Json.obj(), Some(stencil), schemaRef))
        case None => Node(addNode.id, EditorGraphNode(Json.obj(), None, None))
      }

      withNewNode(ctx, nodeToAdd, DefaultGeometry(
        addNode.x.getOrElse(0.0),
        addNode.y.getOrElse(0.0),
        50.0,
        50.0
      ))
  }

  private def withNewNode(ctx: EditorContext,
                          newNode: Node[EditorGraphNode],
                          geometry: DefaultGeometry): EditorContext =
    ctx.updateModel { model =>
      model
        .updateGraph(_.withNode(newNode))
        .updateLayout(_.setNodeGeometry(newNode.id, geometry))
    }.addNotification(this, ElementUpdated(ElementRef(newNode.id, NodeType), Created))

  val handleAddEdge: Transform = ctx => ctx.transform {
    case edge: AddEdge => addEdge(edge)(ctx)
  }

  def addEdge(edge: AddEdge)(ctx: EditorContext): EditorContext = {
    val connector: Option[Connector] =
      edge
        .connectorRef
        .flatMap(ctx.model.palette.findConnector)
        .orElse(ctx.model.palette.connectors.headOption)

    val editorEdge = EditorGraphEdge(
      Json.obj(),
      connector.map(_.id),
      schemaRef = connector.flatMap(_.schemaRef)
    )

    (for {
      fromNode <- ctx.model.graph.findNode(edge.from)
      toNode <- ctx.model.graph.findNode(edge.to)
      newEdge = Edge(edge.id, editorEdge, fromNode.id, toNode.id)
    } yield {
      ctx
        .updateModel(_.updateGraph(_ withEdge newEdge))
        .addNotification(this, ElementUpdated(ElementRef(newEdge.id, EdgeType), Created))
    }).getOrElse(ctx)
  }

  val load: Transform = ctx => ctx.transform {
    case Load(xml, GraphMLFormat) => FromGraphML[Json, Json](xml) match {
      case Right(graphml) =>
        val schemaFromGraphOrDefault: Option[EditorModel.EditorSchema] = {
          import io.circe.generic.auto._

          for {
            schemaResource <- graphml.meta.resources.find(_.typeHint.contains("schema"))
            schema <- JsonSchema.parse[EditorSchemaHints](schemaResource.value).toOption
          } yield schema
        }

        val withNodes = graphml.graph.nodes.foldLeft(ctx.model) {
          case (model, node) =>
            val withNodeAndLayout = model
              .updateSchema(_.merge(schemaFromGraphOrDefault.toList))
              .updateGraph(_.withNode(Node(node.id, EditorGraphNode(node.value.value, None, None, node.value.labelValue))))
              .updateLayout(currentLayout => {
                node.value.geometry.map(nodeGeometry => currentLayout.setNodeGeometry(node.id, nodeGeometry)).getOrElse(currentLayout)
              })

            node.value.shape match {
              case Some(shape) => withNodeAndLayout.updateStyleSheet(_.updateNodeStyle(node.id, _ => Some(shape)))
              case None => withNodeAndLayout
            }
        }

        val withNodesAndEdges = graphml.graph.edges.foldLeft(withNodes) {
          case (model, edge) =>
            edge
              .value
              .shape
              .map(edgeShape =>
                model
                  .updateStyleSheet(_.updateEdgeStyle(edge.id, _ => Some(edgeShape)))
                  .updateLayout(current => edge.value.path.map(edgePath => current.setEdgePath(edge.id, edgePath)).getOrElse(current))
                  .updateGraph(_.withEdge(Edge(edge.id, EditorGraphEdge(edge.value.value, None, None, edge.value.labelValue), edge.from, edge.to)))
              ).getOrElse(model)
        }

        val withGraphAndStyle = graphml.resourcesById.foldLeft(withNodesAndEdges) {
          case (model, (key, GraphMLResource(_, value, typeHint))) => typeHint match {
            case Some("java.awt.image.BufferedImage") =>
              model.updateStyleSheet(_.withImage(key, ImageSpec( s"data:image/png;base64,${unescapeXml(value).replaceAll("\n", "")}", "dataUrl")))
            case _ =>
              model.updateStyleSheet(_.withImage(key, ImageSpec(unescapeXml(value), "svg")))
          }
        }

        ctx
          .updateModel(model => {
            withGraphAndStyle
              .updateStyleSheet(_.merge(model.config.styleSheets))
              .updateSchema(_.merge(model.config.schemas))
          })
          .addNotification(this, Reset)
      case Left(errors) => ctx.addError(this, errors.last)
    }

    case Load(json, JsonFormat) =>
      import EditorGraphJsonFormat._

      io.circe.parser.decode[EditorGraph](json) match {
        case Right(editorGraph) =>
          ctx.updateModel(model => model.copy(
            graph = editorGraph.graph,
            layout = EditorGraphLayouts(editorGraph.layouts.flatMap(_.toOption)),
            styleSheet = StyleSheets(editorGraph.styleSheets.flatMap(_.toOption)) merge model.config.styleSheets,
            schema = EditorSchemas(editorGraph.schemas.flatMap(_.toOption)) merge model.config.schemas)
          ).addNotification(this, Reset)
        case Left(error) => ctx.addError(this, new RuntimeException(error))
      }
  }

  val handleReset: Transform = ctx => ctx.transform {
    case Reset =>
      val newModel = EditorModel.fromConfig(ctx.model.config)
      ctx.copy(model = newModel).addNotification(this, SetModel(newModel))
  }

  val moveNode: Transform = ctx => ctx.transform {
    case move@MoveTo(ElementRef(id, NodeType), x, y) =>
      ctx
        .updateModel(_.updateLayout(_.updateNodePosition(id, _ => x, _ => y)))
        .addNotification(this, ElementUpdated(ElementRef(id, NodeType), Changed, causedBy = Some(move)))

    case move@MoveBy(deltaX, deltaY) =>
      ctx.model.selection.foldLeft(ctx) {
        case (updatedCtx, ElementRef(id, NodeType)) =>
          updatedCtx
            .updateModel(_.updateLayout(_.updateNodePosition(id, _ + deltaX, _ + deltaY)))
            .addNotification(this, ElementUpdated(ElementRef(id, NodeType), Changed, causedBy = Some(move)))
        case (updatedCtx, _) => updatedCtx
      }
  }

  val setNodeLabel: Transform = ctx => ctx.transform {
    case SetLabel(ElementRef(id, EdgeType), text) =>
      ctx
        .updateModel(_.updateGraph(_.updateEdge(id)(_.copy(label = Some(text)))))
        .addNotification(this, ElementUpdated(ElementRef(id, EdgeType)))

    case SetLabel(ElementRef(id, NodeType), text) =>
      ctx
        .updateModel(_.updateGraph(_.updateNode(id)(_.copy(label = Some(text)))))
        .addNotification(this, ElementUpdated(ElementRef(id, NodeType)))

    case SetColor(ElementRef(id, NodeType), color) =>
      ctx
        .updateModel(_.updateStyleSheet(_.updateNodeStyle(id, maybeShape => {
          Some(maybeShape.getOrElse(NodeShape()).copy(fill = maybeShape
            .flatMap(_.fill.map(_.copy(color = Some(color))))
            .orElse(Some(Fill(color = Some(color))))
          ))
        }))).addNotification(this, ElementUpdated(ElementRef(id, NodeType)))
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

  private def updateNodeJson(ctx: EditorContext, id: String, update: Json => Json): EditorContext =
    ctx
      .updateModel(_.updateGraph(_.updateNode(id)(node => node.copy(data = update(node.data)))))
      .addNotification(this, ElementUpdated(ElementRef(id, NodeType)))

  private def updateEdgeJson(ctx: EditorContext, id: String, update: Json => Json): EditorContext =
    ctx
      .updateModel(_.updateGraph(_.updateEdge(id)(edge => edge.copy(data = update(edge.data)))))
      .addNotification(this, ElementUpdated(ElementRef(id, EdgeType)))

  val handleExport: Transform = ctx => ctx.transform {
    case Export(GraphMLFormat) =>
      // FIXME
      ctx.addError(this, new UnsupportedOperationException("graphml export not supported right now"))
    case Export(JsonFormat) =>
      val json = EditorGraphJsonFormat.defaultEditorGraphEncoder(EditorGraph(graph = ctx.model.graph, styleSheets = ctx.model.styleSheet.styleSheets.map(Right(_)), layouts = ctx.model.layout.layouts.map(Right(_)), schemas = ctx.model.schema.schemas.map(Right(_))))

      ctx.addNotification(this, ExportedGraph(
        "graph",
        json.deepDropNullValues.spaces2,
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

  val handleSelect: Transform = ctx => ctx.transform {
    case SelectAll =>
      val allNodes = ctx.model.graph.nodes.map(node => ElementRef(node.id, NodeType)).toSet
      withSelection(ctx, allNodes, append = false)

    case Select(selection, append) =>
      withSelection(ctx, selection, append)
  }

  private def withSelection(ctx: EditorContext, selection: Set[ElementRef], append: Boolean): EditorContext = {
    val oldSelection = ctx.model.selection

    val newSelection = selection.flatMap {
      case ElementRef(id, NodeType) =>
        ctx.model.graph.findNode(id).map(node => ElementRef(node.id, NodeType))
      case ElementRef(id, EdgeType) =>
        ctx.model.graph.findEdge(id).map(edge => ElementRef(edge.id, EdgeType))
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
    case EditorToggle(EditorToggle.connectKey, Some(toggle)) => ctx.copy(model = ctx.model.copy(connectSelection = toggle))
  }

  val handleDelete: Transform = ctx => ctx.transform {
    case DeleteSelection =>
      ctx.model.selection.foldLeft(ctx) {
        case (elementCtx, ElementRef(refId, NodeType)) =>
          val edges = elementCtx.model.graph.incoming(refId) ++ elementCtx.model.graph.outgoing(refId)

          val withoutEdges = edges.foldLeft(elementCtx) {
            case (edgeContext, edge) =>
              edgeContext
                .updateModel(model => model.updateGraph(_.removeEdgeById(edge.id)))
                .addNotification(this, ElementUpdated(ElementRef(edge.id, EdgeType), Deleted))
          }

          withoutEdges
            .updateModel(model => model.updateGraph(_.removeNodeById(refId)))
            .addNotification(this, ElementUpdated(ElementRef(refId, NodeType), Deleted))

        case (context, ElementRef(refId, EdgeType)) =>
          context
            .updateModel(model => model.updateGraph(_.removeEdgeById(refId)))
            .addNotification(this, ElementUpdated(ElementRef(refId, EdgeType), Deleted))
      }
  }

  lazy val handleAll: Transform = handleReset
    .andThen(handleAddEdge)
    .andThen(handleAddNode)
    .andThen(setNodeLabel)
    .andThen(setNodeJson)
    .andThen(moveNode)
    .andThen(load)
    .andThen(handleExport)
    .andThen(handleSelect)
    .andThen(handleConnectToggle)
    .andThen(handleDelete)

  override lazy val eval: Eval = ctx => IO.pure(handleAll(ctx))
}
