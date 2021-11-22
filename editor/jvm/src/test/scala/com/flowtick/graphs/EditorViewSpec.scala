package com.flowtick.graphs

import cats.effect.IO
import com.flowtick.graphs.editor.feature.ModelUpdateFeature
import com.flowtick.graphs.editor.{EditorController, EditorView}

class EditorViewSpec extends EditorBaseSpec {
  "Editor View" should "have higher order than model update" in {
    val viewOrder = new EditorView[Unit, Unit](EditorController()) {
      override def createPage: IO[PageType] = IO.raiseError(new UnsupportedOperationException)
    }.order

    (viewOrder > new ModelUpdateFeature().order) should be(true)
  }
}
