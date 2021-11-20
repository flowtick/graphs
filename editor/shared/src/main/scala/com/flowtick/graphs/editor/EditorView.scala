package com.flowtick.graphs.editor

import cats.effect.IO
import com.flowtick.graphs.view._

abstract class EditorView[T, E](messageBus: EditorMessageBus)
    extends GraphView[T, E, EditorGraphNode, EditorGraphEdge, EditorModel]
    with EditorComponent {
  override def handleSelect(element: ElementRef): Boolean => IO[Unit] = append =>
    for {
      vm <- viewElements.get
      newSelections =
        if (vm.elements.contains(element)) Set(element)
        else Set.empty[ElementRef]
      _ <- messageBus.notifyEvent(this, Select(newSelections, append))
    } yield ()

  def handleDoubleClick: Any => IO[Unit] = (_: Any) => {
    messageBus.publish(EditorToggle(EditorToggle.editKey, Some(true))).void
  }

  def handleDrag(drag: Option[DragStart[T]]): IO[Unit] = (for {
    dragEvent <- drag.filter(value => Math.abs(value.deltaY) > 0 || Math.abs(value.deltaX) > 0)
  } yield messageBus.publish(MoveBy(dragEvent.deltaX, dragEvent.deltaY)).void).getOrElse(IO.unit)

  def handleEvents: Eval = ctx =>
    ctx
      .effect(this) {
        case ResetTransformation => handleResetTransformation

        case Reset => init(ctx.model).void

        case setModel: SetModel => renderGraph(setModel.model)

        case selected: Selected =>
          updateSelections(selected.oldSelection, selected.elements)

        case ElementUpdated(element, Deleted, _) => deleteElementRef(element)

        case ElementUpdated(ElementRef(id, EdgeElementType), _, _) =>
          updateEdge(id, ctx.model).void
        case ElementUpdated(ElementRef(id, NodeElementType), _, _) =>
          updateNode(id, ctx.model).void
      }
      .flatMap(handleCreateNode)

  override lazy val eval: Eval = handleEvents

  def handleCreateNode: Eval = ctx =>
    ctx.transformIO { case create: AddNode =>
      for {
        p <- requirePage
        center = p.pageCenter
      } yield {
        ctx
          .copy(event =
            create.copy(
              x = create.x.orElse(Some(center.x)),
              y = create.y.orElse(Some(center.y))
            )
          )
      }
    }

  override def init(model: EditorModel): IO[Unit] = createAndRender(model)
}
