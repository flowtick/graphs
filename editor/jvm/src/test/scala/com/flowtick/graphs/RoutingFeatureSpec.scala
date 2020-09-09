package com.flowtick.graphs

import java.util.UUID

import cats.effect.IO
import com.flowtick.graphs.graphml.{GraphMLGraph, GraphMLMeta}
import io.circe.Json
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RoutingFeatureSpec extends AnyFlatSpec with Matchers {
  def printView(bus: EditorMessageBus): EditorComponent = new EditorComponent {
    override def init(model: EditorModel): IO[Unit] = IO.unit

    override def eval: Eval = ctx => ctx.effect(this) {
      case SetGraph(graphml) => IO {
        graphml.graph.edges.foreach(println)
      }
      case event => IO(println(event))
    }
  }

  "Routing" should "update edges" in new EditorMain {
    val (firstNodeId, secondNodeId) = ("1", "2")
    val (added, moved) = (for {
      editor <- createEditor(bus => List(
        printView(bus),
        new RoutingFeature(),
        new EditorModelUpdate()
      ))(None, None, None)
      (messageBus, _) = editor
      _ <- messageBus.publish(SetGraph(GraphMLGraph[Json, Json](Graph.empty, GraphMLMeta())))
      edgeId = UUID.randomUUID().toString
      _ <- messageBus.publish(CreateNode(firstNodeId, None))
      _ <- messageBus.publish(CreateNode(secondNodeId, None))
      added <- messageBus.publish(AddEdge(edgeId, firstNodeId, secondNodeId, None))
      moved <- messageBus.publish(Move(ElementRef(firstNodeId, NodeType), 10.0, 10.0))
    } yield (added, moved)).unsafeRunSync()

    moved.model.graphml.graph.edgeIds should have size(1)

    def nodeGeometry(id: String, graphml: GraphMLGraph[Json, Json]) = graphml
    .graph
    .findNode(firstNodeId)
    .flatMap(_.value.shape)
    .flatMap(_.geometry)
    .get

    val posAfterAdd = nodeGeometry(firstNodeId, added.model.graphml)
    posAfterAdd.x should be(0.0)
    posAfterAdd.y should be(0.0)

    val posAfterMove = nodeGeometry(firstNodeId, moved.model.graphml)
    posAfterMove.x should be(10.0)
    posAfterMove.y should be(10.0)
  }
}
