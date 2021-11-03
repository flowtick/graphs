package com.flowtick.graphs.editor

import cats.effect.IO
import cats.effect.unsafe.implicits.global

import cats.implicits._

import com.flowtick.graphs.editor.view.GraphElement
import com.flowtick.graphs.layout.PointSpec
import com.flowtick.graphs.{Edge, Node}
import cats.effect.kernel.Ref

final case class PanContext(
    mouseAnchorX: Double,
    mouseAnchorY: Double,
    translateAnchorX: Double,
    translateAnchorY: Double
)

final case class PagePoint(x: Double, y: Double)

final case class DragStart[T](
    cursorX: Double,
    cursorY: Double,
    transformX: Double,
    transformY: Double,
    dragElem: T,
    element: ElementRef,
    lastPos: Option[PagePoint],
    deltaX: Double,
    deltaY: Double
)

trait Page[T, E] {
  type Point = PagePoint

  var dragStartRef: Ref[IO, Option[DragStart[T]]] = Ref.unsafe(None)

  def root: T
  def pageCenter: PointSpec

  def addEdge(
      edge: Edge[EditorGraphEdge],
      editorModel: EditorModel
  ): IO[Option[GraphElement[T]]]

  def addNode(
      node: Node[EditorGraphNode],
      editorModel: EditorModel
  ): IO[Option[GraphElement[T]]]

  def setSelection(element: GraphElement[T]): IO[Unit]

  def unsetSelection(element: GraphElement[T]): IO[Unit]

  def deleteElement(element: GraphElement[T]): IO[Unit]

  def resetTransformation: IO[Unit]

  def screenCoordinates(x: Double, y: Double): Point

  def pageCoordinates(x: Double, y: Double): Point

  def beforeDrag: E => Unit

  def eventCoordinates(event: E): PagePoint

  def applyDrag: DragStart[T] => Unit

  def drag: E => Unit = evt =>
    dragStartRef
      .update {
        case Some(dragStart: DragStart[_]) =>
          beforeDrag(evt)
          val eventPoint = eventCoordinates(evt)
          val dragCursor = pageCoordinates(eventPoint.x, eventPoint.y)

          val gridSize: Int = 10

          val deltaX = dragCursor.x - dragStart.cursorX
          val deltaY = dragCursor.y - dragStart.cursorY

          val screenPoint = screenCoordinates(
            dragStart.transformX + deltaX,
            dragStart.transformY + deltaY
          )

          val tx = (screenPoint.x.toInt / gridSize) * gridSize
          val ty = (screenPoint.y.toInt / gridSize) * gridSize

          val newDragStart = dragStart.copy(
            lastPos = Some(PagePoint(tx, ty)),
            deltaX = deltaX,
            deltaY = deltaY
          )

          applyDrag(newDragStart)

          Some(newDragStart)
        case None => None
      }
      .unsafeToFuture()

  def endDrag: E => IO[Option[DragStart[T]]] = _ => dragStartRef.getAndUpdate(_ => None)
}

final case class ViewModel[T](graphElements: Map[ElementRef, GraphElement[T]])

trait EditorView[T, E] extends EditorComponent {
  lazy val pageRef: Ref[IO, Option[Page[T, E]]] = Ref.unsafe(None)
  lazy val viewModel: Ref[IO, ViewModel[T]] = Ref.unsafe(ViewModel(Map.empty))

  override def order: Double = 0.4

  def createPage: IO[Page[T, E]]

  def messageBus: EditorMessageBus

  protected def requirePage: IO[Page[T, E]] = pageRef.get.flatMap(
    IO.fromOption(_)(new IllegalStateException("page not set"))
  )

  def handleSelect(element: ElementRef): Boolean => IO[Unit] = append =>
    for {
      vm <- viewModel.get
      newSelections =
        if (vm.graphElements.contains(element)) Set(element)
        else Set.empty[ElementRef]
      _ <- messageBus.notifyEvent(this, Select(newSelections, append))
    } yield ()

  def handleDoubleClick: Any => IO[Unit] = (_: Any) => {
    messageBus.publish(EditorToggle(EditorToggle.editKey, Some(true))).void
  }

  def handleDrag(drag: Option[DragStart[T]]): IO[Unit] = (for {
    dragEvent <- drag.filter(value => Math.abs(value.deltaY) > 0 || Math.abs(value.deltaX) > 0)
  } yield messageBus.publish(MoveBy(dragEvent.deltaX, dragEvent.deltaY)).void)
    .getOrElse(IO.unit)

  def appendEdge(
      editorModel: EditorModel
  )(edge: Edge[EditorGraphEdge]): IO[Option[GraphElement[T]]] = for {
    page <- requirePage
    edgeElement <- page.addEdge(edge, editorModel)

    _ <- {
      def withEdge(vm: ViewModel[T]): ViewModel[T] = edgeElement match {
        case Some(elem) =>
          vm.copy(graphElements = vm.graphElements + (elem.id -> elem))
        case None => vm
      }

      viewModel.update(withEdge)
    }
  } yield edgeElement

  def appendNode(
      editorModel: EditorModel
  )(node: Node[EditorGraphNode]): IO[Option[GraphElement[T]]] = for {
    pageElem <- requirePage
    nodeElement <- pageElem.addNode(node, editorModel)
    _ <- {
      def withNode(vm: ViewModel[T]): ViewModel[T] = nodeElement
        .map(elem => vm.copy(graphElements = vm.graphElements + (elem.id -> elem)))
        .getOrElse(vm)

      viewModel.update(withNode)
    }
  } yield nodeElement

  def updateEdge(id: String, ctx: EditorContext): IO[Option[GraphElement[T]]] =
    ctx.model.graph
      .findEdge(id)
      .map(edge =>
        updateElement(edge, ElementRef(id, EdgeType), ctx.model)(
          appendEdge(ctx.model)
        )
      )
      .getOrElse(IO.pure(None))

  def updateNode(id: String, ctx: EditorContext): IO[Option[GraphElement[T]]] =
    ctx.model.graph
      .findNode(id)
      .map(edge =>
        updateElement(edge, ElementRef(id, NodeType), ctx.model)(
          appendNode(ctx.model)
        )
      )
      .getOrElse(IO.pure(None))

  def updateElement[EL](value: EL, id: ElementRef, model: EditorModel)(
      update: EL => IO[Option[GraphElement[T]]]
  ): IO[Option[GraphElement[T]]] = for {
    vm <- viewModel.get
    p <- requirePage
    updated <- vm.graphElements.get(id) match {
      case Some(element: GraphElement[T]) =>
        p.deleteElement(element) *> update(value).flatTap {
          case Some(updatedElement) =>
            if (model.selection.contains(element.id)) {
              p.setSelection(updatedElement)
            } else IO.unit

          case None => IO.unit
        }

      case Some(other) =>
        IO.raiseError(
          new IllegalStateException(
            s"did not find node for id in view model, found: $other"
          )
        )

      case None => update(value)
    }
  } yield updated

  override def init(model: EditorModel): IO[Unit] =
    for {
      newPage <- createPage
      _ <- pageRef.set(Some(newPage))
      rendered <- renderGraph(model)
    } yield rendered

  def renderGraph(editorModel: EditorModel): IO[Unit] = for {
    vm <- viewModel.get
    _ <- vm.graphElements.keys.map(deleteElementRef).toList.sequence
    _ <- editorModel.graph.nodes.map(appendNode(editorModel)(_)).toList.sequence
    _ <- editorModel.graph.edges.map(appendEdge(editorModel)(_)).toList.sequence
  } yield ()

  def handleEvents: Eval = ctx =>
    ctx
      .effect(this) {
        case ResetTransformation => handleResetTransformation

        case Reset => init(ctx.model).void

        case setModel: SetModel => renderGraph(setModel.model)

        case selected: Selected =>
          updateSelections(selected.oldSelection, selected.elements)

        case ElementUpdated(element, Deleted, _) => deleteElementRef(element)

        case ElementUpdated(ElementRef(id, EdgeType), _, _) =>
          updateEdge(id, ctx).void
        case ElementUpdated(ElementRef(id, NodeType), _, _) =>
          updateNode(id, ctx).void
      }
      .flatMap(handleCreateNode)

  def handleResetTransformation: IO[Unit] = for {
    p <- requirePage
    _ <- p.resetTransformation
  } yield ()

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

  def updateSelections(
      oldSelections: Set[ElementRef],
      newSelections: Set[ElementRef]
  ): IO[Unit] =
    for {
      vm <- viewModel.get
      p <- requirePage
      _ <- IO {
        // TODO: FP
        oldSelections.foreach(elem =>
          vm.graphElements
            .get(elem)
            .foreach(p.unsetSelection(_).unsafeToFuture())
        )
        newSelections.foreach(elem =>
          vm.graphElements.get(elem).foreach(p.setSelection(_).unsafeToFuture())
        )
      }
    } yield ()

  def deleteElementRef(elementRef: ElementRef): IO[Unit] = for {
    vm <- viewModel.get
    p <- requirePage
    _ <- vm.graphElements
      .get(elementRef)
      .map(element => p.deleteElement(element))
      .getOrElse(IO.unit)
    _ <- viewModel.update(vm => vm.copy(graphElements = vm.graphElements - elementRef))
  } yield ()
}
