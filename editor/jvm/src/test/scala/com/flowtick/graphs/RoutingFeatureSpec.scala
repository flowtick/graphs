package com.flowtick.graphs

import java.io.FileOutputStream
import java.util.UUID

import cats.effect.IO
import com.flowtick.graphs.editor.feature.RoutingFeature
import com.flowtick.graphs.editor._
import com.flowtick.graphs.graphml.GraphMLGraph
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

      case ExportedGraph(name, value, format) => IO {
        val out = new FileOutputStream(s"target/last_test$name" + format.`extension`)
        out.write(value.getBytes("UTF-8"))
        out.flush()
        out.close()
      }

      case event => IO(println(event))
    }
  }

  "Routing" should "update edges" in new EditorMain {
    val (firstNodeId, secondNodeId) = ("1", "2")
    val edgeId = UUID.randomUUID().toString
    val (added, moved) = (for {
      editor <- createEditor(bus => List(
        printView(bus),
        new EditorModelUpdate,
        new RoutingFeature()
      ))(EditorOptions())
      (messageBus, _) = editor
      _ <- messageBus.publish(SetGraph(EditorGraph.empty))

      _ <- messageBus.publish(CreateNode(firstNodeId, None, Some(100.0), Some(100.0)))
      _ <- messageBus.publish(CreateNode(secondNodeId, None, Some(200.0), Some(200.0)))
      added <- messageBus.publish(AddEdge(edgeId, firstNodeId, secondNodeId, None))
      moved <- messageBus.publish(MoveTo(ElementRef(firstNodeId, NodeType), 110.0, 110.0))
      _ <- messageBus.publish(Export(JsonFormat))
    } yield (added, moved)).unsafeRunSync()

    moved.model.editorGraph.graph.edgeIds should have size(1)

    def nodeGeometry(graphml: GraphMLGraph[Json, Json]) = graphml
    .graph
    .findNode(firstNodeId)
    .flatMap(_.value.geometry)
    .get

    val posAfterAdd = added.model.editorGraph.layout.nodes(firstNodeId)
    posAfterAdd.x should be(100.0)
    posAfterAdd.y should be(100.0)

    val posAfterMove = moved.model.editorGraph.layout.nodes(firstNodeId)
    posAfterMove.x should be(110.0)
    posAfterMove.y should be(110.0)

    val edgeAfterMove = moved.model.editorGraph.graph.findEdge(edgeId)
    edgeAfterMove.isDefined should be(true)
  }
}
