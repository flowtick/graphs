package com.flowtick.graphs.editor

import cats.effect.IO
import cats.effect.concurrent.Ref
import cats.implicits._
import com.flowtick.graphs.Graph
import com.flowtick.graphs.editor.feature.{ModelUpdateFeature, RoutingFeature, UndoFeature}
import com.flowtick.graphs.style._

final case class EditorInstance(bus: EditorMessageBus, components: List[EditorComponent])

trait EditorMain {
  def createEditor(additionalComponents: EditorMessageBus => List[EditorComponent])(
    configuration: EditorConfiguration): IO[EditorInstance] = for {
    listeners <- Ref.of[IO, List[EditorComponent]](List.empty)
    log <- Ref.of[IO, List[EditorEvent]](List.empty)

    messageBus <- IO.pure(new EditorController(
      log,
      listeners,
      initial = EditorModel.fromConfig(configuration)
    ))

    components = List(new ModelUpdateFeature, new RoutingFeature, new UndoFeature) ++ additionalComponents(messageBus)

    subscribed <- components.sortBy(_.order).map(component => {
      messageBus.subscribe(component) *> IO(component)
    }).sequence
  } yield EditorInstance(messageBus, subscribed)
}
