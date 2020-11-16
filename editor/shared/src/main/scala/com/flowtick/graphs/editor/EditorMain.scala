package com.flowtick.graphs.editor

import cats.effect.IO
import cats.effect.concurrent.Ref
import cats.implicits._
import com.flowtick.graphs.Graph
import com.flowtick.graphs.editor.feature.{RoutingFeature, UndoFeature}
import com.flowtick.graphs.style.{BorderStyle, EdgeLabel, EdgeShape, EdgeStyle, Fill, NodeLabel, NodeShape, ShapeType, StyleSheet}
import com.flowtick.graphs.json.schema.Schema

trait EditorMain {
  lazy val defaultNodeShape = NodeShape(
    shapeType = Some(ShapeType.RoundRectangle),
    labelStyle = Some(NodeLabel(textColor = Some("#000000"))),
    fill = Some(Fill(color = Some("#FFFFFF"))),
    borderStyle = Some(BorderStyle("#CCC", None, Some(1.0)))
  )

  lazy val defaultEdgeShape = EdgeShape(
    edgeStyle = Some(EdgeStyle(color = "#000000")),
    labelStyle = Some(EdgeLabel(textColor = Some("#000000")))
  )

  lazy val defaultStyleSheet =
    StyleSheet
      .empty
      .copy(defaultNode = Some(defaultNodeShape), defaultEdge = Some(defaultEdgeShape))

  def createEditor(additionalComponents: EditorMessageBus => List[EditorComponent])(
                   options: EditorOptions): IO[(EditorMessageBus, List[EditorComponent])] = for {
    listeners <- Ref.of[IO, List[EditorComponent]](List.empty)
    log <- Ref.of[IO, List[EditorEvent]](List.empty)
    graph = options.initial.map(_.graph).getOrElse(Graph.empty)
    editorGraph = EditorGraph(graph, options.styleSheet.getOrElse(defaultStyleSheet), EditorGraphLayout(), options.schema.getOrElse(Schema[EditorSchemaHints]()))

    messageBus <- IO.pure(new EditorController(
      log,
      listeners,
      initial = editorGraph,
      palette = options.palette
    ))
    components = List(new EditorModelUpdate, new RoutingFeature, new UndoFeature) ++ additionalComponents(messageBus)

    subscribed <- components.sortBy(_.order).map(component => {
      messageBus.subscribe(component) *> IO(component)
    }).sequence

    _ <- messageBus.publish(SetGraph(editorGraph))

  } yield (messageBus, subscribed)

}
