package com.flowtick.graphs

import cats.effect.IO
import cats.effect.concurrent.Ref
import cats.implicits._
import com.flowtick.graphs.graphml.GraphMLGraph
import com.flowtick.graphs.json.schema.Schema
import io.circe.Json

trait EditorMain {
  def createEditor(components: EditorMessageBus => List[EditorComponent])(
                   initalGraph: Option[GraphMLGraph[Json, Json]],
                   palette: Option[Palette],
                   schema: Option[EditorModel.EditorSchema]): IO[(EditorMessageBus, List[EditorComponent])] = for {
    listeners <- Ref.of[IO, List[EditorComponent]](List.empty)
    log <- Ref.of[IO, List[EditorEvent]](List.empty)
    messageBus <- IO.pure(new EditorController(log, listeners, graphml = initalGraph, palette = palette, schema = schema))

    subscribed <- components(messageBus).map(component => {
      messageBus.subscribe(component) *> IO(component)
    }).sequence

  } yield (messageBus, subscribed)

}
