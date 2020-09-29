package com.flowtick.graphs.editor

import cats.effect.IO
import cats.effect.concurrent.Ref
import cats.implicits._
import com.flowtick.graphs.editor.feature.{RoutingFeature, UndoFeature}
import com.flowtick.graphs.graphml.{GraphMLGraph, GraphMLMeta}

trait EditorMain {

  def createEditor(additionalComponents: EditorMessageBus => List[EditorComponent])(
                   options: EditorOptions): IO[(EditorMessageBus, List[EditorComponent])] = for {
    listeners <- Ref.of[IO, List[EditorComponent]](List.empty)
    log <- Ref.of[IO, List[EditorEvent]](List.empty)
    messageBus <- IO.pure(new EditorController(log, listeners, graphml = options.initial.map(json => GraphMLGraph(json.graph, GraphMLMeta())), palette = options.palette, schema = options.schema))
    components = List(new EditorModelUpdate, new RoutingFeature, new UndoFeature) ++ additionalComponents(messageBus)

    subscribed <- components.sortBy(_.order).map(component => {
      messageBus.subscribe(component) *> IO(component)
    }).sequence

  } yield (messageBus, subscribed)

}
