package com.flowtick.graphs

import cats.effect.IO
import com.flowtick.graphs.editor._
import com.flowtick.graphs.layout.{DefaultGeometry, EdgePath}

class ModelUpdateFeatureSpec extends EditorBaseSpec {
  override def shouldRenderImage: Boolean = true

  "Editor Model Update" should "add nodes and edges" in withEditor { editor =>
    val (firstNodeId, secondNodeId) = ("1", "2")
    val edgeId = "e1"

    val loaded = (for {
      _ <- editor.bus.publish(Reset)
      _ <- editor.bus.publish(AddNode(firstNodeId, None, Some(100.0), Some(100.0)))
      _ <- editor.bus.publish(AddNode(secondNodeId, None, Some(200.0), Some(200.0)))
      _ <- editor.bus.publish(SetLabel(ElementRef(firstNodeId, NodeType), "1"))
      _ <- editor.bus.publish(SetLabel(ElementRef(secondNodeId, NodeType), "2"))
      _ <- editor.bus.publish(AddEdge(edgeId, firstNodeId, secondNodeId, None))
      _ <- editor.bus.publish(Export(JsonFormat))
      exported <- lastExported.get.flatMap(IO.fromOption(_)(new IllegalStateException("exported graph not set")))
      loaded <- editor.bus.publish(Load(exported.value, JsonFormat))
    } yield loaded).unsafeRunSync()

    loaded.model.graph.nodes should have size(2)
    loaded.model.graph.edges should have size(1)

    loaded.model.layout.nodeGeometry(firstNodeId) should be(Some(DefaultGeometry(100.0, 100.0, 50.0, 50.0)))
    loaded.model.layout.edgePath(edgeId) should be(Some(EdgePath(25.0, 25.0, -25.0, -25.0, List.empty)))
  }

  it should "update the color of a node" in withEditor { editor =>
    val firstNodeId = "1"

    val color = "#ccc"

    val loaded = (for {
      _ <- editor.bus.publish(Reset)
      _ <- editor.bus.publish(AddNode(firstNodeId, None, Some(100.0), Some(100.0)))
      _ <- editor.bus.publish(SetColor(ElementRef("1", NodeType), color))
      _ <- editor.bus.publish(Export(JsonFormat))
      exported <- lastExported.get.flatMap(IO.fromOption(_)(new IllegalStateException("exported graph not set")))
      loaded <- editor.bus.publish(Load(exported.value, JsonFormat))
    } yield loaded).unsafeRunSync()

    loaded.model.styleSheet.getNodeStyle(Some("1")).flatMap(_.fill).flatMap(_.color) should be(Some(color))
  }
}
