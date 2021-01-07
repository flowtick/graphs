package com.flowtick.graphs.editor.feature

import cats.effect.IO
import com.flowtick.graphs.editor._

import scala.collection.mutable.ListBuffer

class UndoFeature extends EditorComponent {
  val modelHistory = ListBuffer.empty[EditorModel]

  override def eval: Eval = ctx => ctx.effect(this) {
    case Reset => IO {
      modelHistory.prepend(ctx.model)
    }

    case ElementUpdated(ElementRef(_, _), updateType, _) if updateType != Internal => IO {
      if (modelHistory.isEmpty || modelHistory.headOption.exists(lastModel => lastModel.version < ctx.model.version)) {
        modelHistory.prepend(ctx.model)
      }
    }
  }.flatMap(_.transformIO {
    case Undo => IO {
      if(modelHistory.length > 1) {
        modelHistory.remove(0)
        modelHistory.headOption match {
          case Some(previousModel) =>
            ctx.addCommand(SetModel(previousModel.copy(selection = Set.empty)))
          case None => ctx
        }

      } else ctx
    }
  })
}
