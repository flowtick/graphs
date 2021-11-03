package com.flowtick.graphs

import com.flowtick.graphs.editor._

class RoutingFeatureSpec extends EditorBaseSpec {
  "Routing" should "update edges" in withEditor { editor =>
    val (firstNodeId, secondNodeId) = ("1", "2")
    val edgeId = "e1"

    val (added, moved) = (for {
      _ <- editor.bus.publish(Reset)
      _ <- editor.bus.publish(
        AddNode(firstNodeId, None, Some(100.0), Some(100.0))
      )
      _ <- editor.bus.publish(
        AddNode(secondNodeId, None, Some(200.0), Some(200.0))
      )
      added <- editor.bus.publish(
        AddEdge(edgeId, firstNodeId, secondNodeId, None)
      )
      moved <- editor.bus.publish(
        MoveTo(ElementRef(firstNodeId, NodeType), 110.0, 110.0)
      )
      _ <- editor.bus.publish(Export(JsonFormat))
    } yield (added, moved)).unsafeToFuture()

    moved.model.graph.edgeIds should have size (1)

    added.model.layout.nodeGeometry(firstNodeId) match {
      case Some(posAfterAdd) =>
        posAfterAdd.x should be(100.0)
        posAfterAdd.y should be(100.0)
      case None => fail()
    }

    moved.model.layout.nodeGeometry(firstNodeId) match {
      case Some(posAfterMove) =>
        posAfterMove.x should be(110.0)
        posAfterMove.y should be(110.0)
      case None => fail()
    }

    val edgeAfterMove = moved.model.graph.findEdge(edgeId)
    edgeAfterMove.isDefined should be(true)
  }
}
