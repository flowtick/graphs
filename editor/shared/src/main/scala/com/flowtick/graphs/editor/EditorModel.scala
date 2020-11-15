package com.flowtick.graphs.editor

import java.util.UUID

import cats.data.Validated.{Valid, _}
import cats.data.ValidatedNel
import cats.effect.IO
import com.flowtick.graphs.editor.ImageLoader.unescapeXml
import com.flowtick.graphs.graphml.{Datatype, DatatypeString, FromGraphML, GraphMLGraph, GraphMLKey, GraphMLResource, graphMLDataType}
import com.flowtick.graphs.style._
import com.flowtick.graphs.json.schema.{JsonSchema, Schema}
import com.flowtick.graphs.layout.{DefaultGeometry, Geometry}
import com.flowtick.graphs.{Edge, Graph, Node}
import io.circe.{Json, _}

import scala.xml.NodeSeq

class EditorModelUpdate extends EditorComponent {

  override def order: Double = 0.1

  implicit val jsonDataType: Datatype[Json] = EditorModel.jsonDataType(emptyOnError = true)

  val handleAddNode: Transform = ctx => ctx.transform {
    case add: CreateNode =>
      val allStencils = ctx.model.palette.stencils.flatMap(_.items)
      val addStencil = add.stencilRef.flatMap(id => allStencils.find(_.id == id))

      addStencil
        .orElse(allStencils.headOption)
        .flatMap { stencil =>
          for {
            xPos <- add.x.orElse(Some(0.0))
            yPos <- add.y.orElse(Some(0.0))
          } yield stencil match {
            case Stencil(stencil, _, _, schemaRef) =>
              withNewNode(ctx, Node(add.id, EditorGraphNode(Json.obj(), Some(stencil), schemaRef)), DefaultGeometry(
                xPos,
                yPos,
                50.0,
                50.0
              ))
            case _ => ctx
          }
      }.getOrElse(ctx)
  }

  private def withNewNode(ctx: EditorContext,
                          newNode: Node[EditorGraphNode],
                          geometry: DefaultGeometry): EditorContext = {
    val updatedGraph = ctx.model.editorGraph.copy(
      graph = ctx.model.editorGraph.graph withNode newNode,
      layout = ctx.model.editorGraph.layout.setNodeGeometry(newNode.id, geometry)
    )

    ctx
      .copy(model = ctx.model.updateGraph(updatedGraph))
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

    val editorEdge = EditorGraphEdge(
      Json.obj(),
      connector.map(_.id),
      schemaRef = connector.flatMap(_.schemaRef)
    )

    (for {
      fromNode <- ctx.model.editorGraph.graph.findNode(edge.from)
      toNode <- ctx.model.editorGraph.graph.findNode(edge.to)
      newEdge = Edge(edge.id, editorEdge, fromNode.id, toNode.id)
    } yield {
      val updated = ElementUpdated(ElementRef(newEdge.id, EdgeType), Created)
      val graphMl = ctx.model.updateGraph(ctx.model.editorGraph.copy(graph = ctx.model.editorGraph.graph withEdge newEdge))
      ctx
        .copy(model = graphMl)
        .addNotification(this, updated)
    }).getOrElse(ctx)
  }

  val load: Transform = ctx => ctx.transform {
    case Load(xml, GraphMLFormat) => FromGraphML[Json, Json](xml) match {
      case Right(graphml) =>
        val schemaFromGraphOrEmpty: EditorModel.EditorSchema = {
          import io.circe.generic.auto._

          for {
            schemaResource <- graphml.meta.resources.find(_.typeHint.contains("schema"))
            schema <- JsonSchema.parse[EditorSchemaHints](schemaResource.value).toOption
          } yield schema
        }.getOrElse(Schema())

        val emptyEditorGraph = EditorGraph.empty.copy(
          schema = schemaFromGraphOrEmpty,
          styleSheet = StyleSheet.empty
        )

        val withNodes: EditorGraph = graphml.graph.nodes.foldLeft(emptyEditorGraph) {
          case (editorGraph, node) =>
            val withNodeAndLayout = editorGraph
              .copy(
                graph = editorGraph.graph.withNode(Node(node.id, EditorGraphNode(node.value.value, None, None, node.value.labelValue))),
                layout = node.value.geometry
                  .map(nodeGeometry => editorGraph.layout.setNodeGeometry(node.id, nodeGeometry))
                  .getOrElse(editorGraph.layout)
              )

            node.value.shape match {
              case Some(shape) =>
                withNodeAndLayout.copy(
                  styleSheet = withNodeAndLayout.styleSheet.updateNodeStyle(node.id, _ => Some(shape))
                )
              case None => withNodeAndLayout
            }
        }

        val withNodesAndEdges = graphml.graph.edges.foldLeft(withNodes) {
          case (editorGraph, edge) =>
            val editorEdge = Edge(edge.id, EditorGraphEdge(edge.value.value, None, None, edge.value.labelValue), edge.from, edge.to)

            edge
              .value
              .shape
              .map(edgeShape => editorGraph.copy(
                styleSheet = editorGraph.styleSheet.updateEdgeStyle(edge.id, _ => Some(edgeShape)),
                layout = edge.value.path.map(edgePath => editorGraph.layout.setEdgePath(edge.id, edgePath)).getOrElse(editorGraph.layout)
              )).getOrElse(editorGraph).copy(graph = editorGraph.graph.withEdge(editorEdge))
        }

        // TODO: copy image resources to stylesheet

        val styleSheetWithImages = graphml.resourcesById.foldLeft(withNodesAndEdges.styleSheet) {
          case (styleSheet, (key, GraphMLResource(_, value, typeHint))) => typeHint match {
            case Some("java.awt.image.BufferedImage") =>
              styleSheet.withImage(key, ImageSpec( s"data:image/png;base64,${unescapeXml(value).replaceAll("\n", "")}", "dataUrl"))
            case _ =>
              styleSheet.withImage(key, ImageSpec(unescapeXml(value), "svg"))
          }
        }

        val newGraph = withNodesAndEdges.copy(styleSheet = styleSheetWithImages)

        ctx.copy(
          model = ctx.model.updateGraph(newGraph)
        ).addNotification(this, SetGraph(newGraph))
      case Left(errors) => ctx.addError(this, errors.last)
    }

    case Load(json, JsonFormat) =>
      import EditorGraphJsonFormat._

      io.circe.parser.decode[EditorGraph](json) match {
        case Right(editorGraph) =>
          ctx.copy(
            model = ctx.model.updateGraph(editorGraph)
          ).addNotification(this, SetGraph(editorGraph))
        case Left(error) => ctx.addError(this, new RuntimeException(error))
      }
    case SetGraph(newGraph) =>
      ctx.copy(model = ctx.model.updateGraph(newGraph))
  }

  val setModel: Transform = ctx => ctx.transform {
    case SetModel(model) => ctx.copy(model = model)
  }

  val moveNode: Transform = ctx => ctx.transform {
    case move@MoveTo(ElementRef(id, NodeType), x, y) =>
      val updatedGraph = ctx.model.editorGraph.updateLayout(_.updateNodePosition(id, _ => x, _ => y))

      ctx
        .copy(model = ctx.model.updateGraph(updatedGraph))
        .addNotification(this, ElementUpdated(ElementRef(id, NodeType), Changed, causedBy = Some(move)))

    case move@MoveBy(deltaX, deltaY) =>
      ctx.model.selection.foldLeft(ctx) {
        case (updatedCtx, ElementRef(id, NodeType)) =>
          val updatedGraph = updatedCtx.model.editorGraph.copy(layout = updatedCtx.model.editorGraph.layout.updateNodePosition(id, _ + deltaX, _ + deltaY))

          updatedCtx
            .copy(model = updatedCtx.model.updateGraph(updatedGraph))
            .addNotification(this, ElementUpdated(ElementRef(id, NodeType), Changed, causedBy = Some(move)))

        case (updatedCtx, _) => updatedCtx
      }
  }

  val setNodeLabel: Transform = ctx => ctx.transform {
    case SetLabel(ElementRef(id, EdgeType), text) =>
      val updatedGraph = ctx.model.editorGraph.graph.updateEdge(id)(_.copy(label = Some(text)))

      ctx
        .copy(model = ctx.model.updateGraph(ctx.model.editorGraph.copy(graph = updatedGraph)))
        .addNotification(this, ElementUpdated(ElementRef(id, EdgeType)))

    case SetLabel(ElementRef(id, NodeType), text) =>
      val updatedGraph = ctx.model.editorGraph.graph.updateNode(id)(_.copy(label = Some(text)))

      ctx
        .copy(model = ctx.model.updateGraph(ctx.model.editorGraph.copy(graph = updatedGraph)))
        .addNotification(this, ElementUpdated(ElementRef(id, NodeType)))

    case SetColor(ElementRef(id, NodeType), color) =>
      val updatedGraph = ctx.model.editorGraph.updateStyleSheet(_.updateNodeStyle(id, maybeShape => {
        Some(maybeShape.getOrElse(NodeShape()).copy(fill = maybeShape
          .flatMap(_.fill.map(_.copy(color = Some(color))))
          .orElse(Some(Fill(color = Some(color))))
        ))
      }))

      ctx
        .copy(model = ctx.model.updateGraph(updatedGraph))
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
    val updatedGraph = ctx.model.editorGraph.graph.updateNode(id)(node => node.copy(data = update(node.data)))

    ctx
      .copy(model = ctx.model.updateGraph(ctx.model.editorGraph.copy(graph = updatedGraph)))
      .addNotification(this, ElementUpdated(ElementRef(id, NodeType)))
  }

  private def updateEdgeJson(ctx: EditorContext, id: String, update: Json => Json): EditorContext = {
    val updatedGraph = ctx.model.editorGraph.graph.updateEdge(id)(edge => edge.copy(data = update(edge.data)))

    ctx
      .copy(model = ctx.model.updateGraph(ctx.model.editorGraph.copy(graph = updatedGraph)))
      .addNotification(this, ElementUpdated(ElementRef(id, EdgeType)))
  }

  val handleExport: Transform = ctx => ctx.transform {
    case Export(GraphMLFormat) =>
      // FIXME
      ctx.addError(this, new UnsupportedOperationException("graphml export not supported right now"))
    case Export(JsonFormat) =>
      val json = EditorGraphJsonFormat.defaultEditorGraphEncoder(ctx.model.editorGraph).spaces2

      ctx.addNotification(this, ExportedGraph(
        "graph",
        json,
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
      val allNodes = ctx.model.editorGraph.graph.nodes.map(node => ElementRef(node.id, NodeType)).toSet
      withSelection(ctx, allNodes, append = false)

    case Select(selection, append) =>
      withSelection(ctx, selection, append)
  }

  private def withSelection(ctx: EditorContext, selection: Set[ElementRef], append: Boolean): EditorContext = {
    val oldSelection = ctx.model.selection

    val newSelection = selection.flatMap {
      case ElementRef(id, NodeType) =>
        ctx.model.editorGraph.graph.findNode(id).map(node => ElementRef(node.id, NodeType))
      case ElementRef(id, EdgeType) =>
        ctx.model.editorGraph.graph.findEdge(id).map(edge => ElementRef(edge.id, EdgeType))
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
        case (elementCtx, ref) if ref.elementType == NodeType =>
          val edges = elementCtx.model.editorGraph.graph.incoming(ref.id) ++ elementCtx.model.editorGraph.graph.outgoing(ref.id)

          val withoutEdges = edges.foldLeft(elementCtx) {
            case (edgeContext, edge) =>
              edgeContext
                .updateModel(model => model.copy(editorGraph = model.editorGraph.removeEdge(edge.id)))
                .addNotification(this, ElementUpdated(ElementRef(edge.id, EdgeType), Deleted))
          }

          withoutEdges
            .updateModel(model => model.copy(editorGraph = model.editorGraph.removeNode(ref.id)))
            .addNotification(this, ElementUpdated(ElementRef(ref.id, NodeType), Deleted))

        case (context, ref) if ref.elementType == EdgeType =>
          context
            .updateModel(model => model.copy(editorGraph = model.editorGraph.removeEdge(ref.id)))
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

final case class EditorGraphLayout(nodes: Map[String, Geometry] = Map.empty,
                                   edges: Map[String, EdgePath] = Map.empty) {
  def setNodeGeometry(id: String, geometry: Geometry): EditorGraphLayout =
    copy(nodes = nodes + (id -> geometry))

  def updateNodePosition(id: String, fx: Double => Double, fy: Double => Double): EditorGraphLayout =
    copy(nodes = nodes.get(id).map(geo => nodes + (id -> DefaultGeometry(x = fx(geo.x), y = fy(geo.y), geo.width, geo.height))).getOrElse(nodes))

  def setEdgePath(id: String, edgePath: EdgePath): EditorGraphLayout =
    copy(edges = edges + (id -> edgePath))
}

trait EditorGraphElement {
  def data: Json
  def label: Option[String]
  def schemaRef: Option[String]
}

final case class EditorGraphNode(data: Json,
                                 stencil: Option[String],
                                 schemaRef: Option[String],
                                 label: Option[String] = None) extends EditorGraphElement

final case class EditorGraphEdge(data: Json,
                                 connector: Option[String],
                                 schemaRef: Option[String],
                                 label: Option[String] = None) extends EditorGraphElement

final case class EditorGraph(graph: Graph[EditorGraphEdge, EditorGraphNode],
                             styleSheet: StyleSheet,
                             layout: EditorGraphLayout,
                             schema: EditorModel.EditorSchema) {
  def updateNode(id: String, update: EditorGraphNode => EditorGraphNode): EditorGraph =
    copy(graph = graph.updateNode(id)(update))

  def updateEdge(id: String, update: EditorGraphEdge => EditorGraphEdge): EditorGraph =
    copy(graph = graph.updateEdge(id)(update))

  def updateStyleSheet(update: StyleSheet => StyleSheet): EditorGraph =
    copy(styleSheet = update(styleSheet))

  def updateLayout(update: EditorGraphLayout => EditorGraphLayout): EditorGraph =
    copy(layout = update(layout))

  def updateSchema(update: EditorModel.EditorSchema => EditorModel.EditorSchema): EditorGraph =
    copy(schema = update(schema))

  def removeNode(id: String): EditorGraph = copy(graph = graph.removeNodeById(id))
  def removeEdge(id: String): EditorGraph = copy(graph = graph.removeEdgeById(id))
}

object EditorGraph {
  val empty: EditorGraph = EditorGraph(Graph.empty, StyleSheet.empty, EditorGraphLayout(), Schema[EditorSchemaHints]())
}


final case class EditorModel(editorGraph: EditorGraph,
                             selection: Set[ElementRef] = Set.empty,
                             connectSelection: Boolean = false,
                             palette: Palette,
                             version: Long = 0) {
  def updateGraph(updated: EditorGraph): EditorModel = copy(editorGraph = updated, version = version + 1)

  override def toString: String = {
    s"""version = $version, connect = $connectSelection, selection = $selection"""
  }
}

final case class EditorSchemaHints(copyToLabel: Option[Boolean] = None,
                                   hideLabelProperty: Option[Boolean] = None,
                                   highlight: Option[String] = None,
                                   showJsonProperty: Option[Boolean] = None)

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
