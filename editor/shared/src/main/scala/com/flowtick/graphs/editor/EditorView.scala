package com.flowtick.graphs.editor

import cats.effect.IO
import cats.effect.concurrent.Ref
import cats.implicits._
import com.flowtick.graphs.graphml.{GraphMLEdge, GraphMLGraph, GraphMLNode, PointSpec}
import com.flowtick.graphs.{Edge, Node}
import io.circe.Json

final case class PanContext(mouseAnchorX: Double, mouseAnchorY: Double, translateAnchorX: Double, translateAnchorY: Double)

final case class DragStart[T](cursorX: Double,
                              cursorY: Double,
                              transformX: Double,
                              transformY: Double,
                              dragElem: T,
                              element: ElementRef,
                              lastPos: Option[(Double, Double)],
                              deltaX: Double,
                              deltaY: Double)

trait Page[T] {
  def root: T
  def pageCenter: PointSpec

  def addEdge(edge: Edge[GraphMLEdge[Json]],
              graphml: GraphMLGraph[Json, Json]): IO[Option[GraphElement[T]]]

  def addNode(node: Node[GraphMLNode[Json]], graphml: GraphMLGraph[Json, Json]): IO[Option[GraphElement[T]]]

  def setSelection(element: GraphElement[T]): IO[Unit]

  def unsetSelection(element: GraphElement[T]): IO[Unit]

  def deleteElement(element: GraphElement[T]): IO[Unit]

  def resetTransformation: IO[Unit]
}

trait GraphElement[+T] {
  def id: ElementRef
  def group: T
  def selectElem: T
  def label: T
}

final case class ViewModel[T](graphElements: Map[ElementRef, GraphElement[T]])

trait EditorView[T] extends EditorComponent {
  lazy val page = Ref.unsafe[IO, Page[T]](createPage.unsafeRunSync())
  lazy val viewModel: Ref[IO, ViewModel[T]] = Ref.unsafe(ViewModel(Map.empty))

  override def order: Double = 0.4

  def createPage: IO[Page[T]]

  def messageBus: EditorMessageBus

  def handleSelect(element: ElementRef): Boolean => IO[Unit] = append => for {
    vm <- viewModel.get
    newSelections =
      if(vm.graphElements.contains(element)) Set(element)
      else Set.empty[ElementRef]
    _ <- messageBus.notifyEvent(this, Select(newSelections, append))
  } yield ()

  def handleDoubleClick: Any => IO[Unit] = (_: Any) => {
    messageBus.publish(Toggle(Toggle.editKey, Some(true))).void
  }

  def handleDrag(drag: Option[DragStart[T]]): IO[Unit] = (for {
    dragEvent <- drag.filter(value => Math.abs(value.deltaY) > 0 || Math.abs(value.deltaX) > 0)
  } yield messageBus.publish(MoveBy(dragEvent.deltaX, dragEvent.deltaY)).void).getOrElse(IO.unit)

  def appendEdge(graphml: GraphMLGraph[Json, Json])(edge: Edge[GraphMLEdge[Json]]): IO[Option[GraphElement[T]]] = for {
    page <- page.get

    edgeElement <- page.addEdge(edge, graphml)

    _ <- {
      def withEdge(vm: ViewModel[T]): ViewModel[T] = edgeElement match {
        case Some(elem) => vm.copy(graphElements = vm.graphElements + (elem.id -> elem))
        case None => vm
      }

      viewModel.update(withEdge)
    }
  } yield edgeElement

  def appendNode(graphml: GraphMLGraph[Json, Json])(node: Node[GraphMLNode[Json]]): IO[Option[GraphElement[T]]] = for {
    pageElem <- page.get
    nodeElement <- pageElem.addNode(node, graphml)
    _ <- {
      def withNode(vm: ViewModel[T]): ViewModel[T] = nodeElement
        .map(elem => vm.copy(graphElements = vm.graphElements + (elem.id -> elem)))
        .getOrElse(vm)

      viewModel.update(withNode)
    }
  } yield nodeElement

  def updateEdge(id: String, ctx: EditorContext): IO[Option[GraphElement[T]]] =
    ctx.model.graphml.graph
      .findEdge(id)
      .map(edge => updateElement(edge, ElementRef(id, EdgeType), ctx.model)(appendEdge(ctx.model.graphml)))
      .getOrElse(IO.pure(None))

  def updateNode(id: String, ctx: EditorContext): IO[Option[GraphElement[T]]] =
    ctx.model.graphml.graph
      .findNode(id)
      .map(edge => updateElement(edge, ElementRef(id, NodeType), ctx.model)(appendNode(ctx.model.graphml)))
      .getOrElse(IO.pure(None))

  def updateElement[E](value: E,
                       id: ElementRef,
                       model: EditorModel)(update: E => IO[Option[GraphElement[T]]]): IO[Option[GraphElement[T]]] = for {
    vm <- viewModel.get
    p <- page.get
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
        IO.raiseError(new IllegalStateException(s"did not find node for id in view model, found: $other"))

      case None => update(value)
    }
  } yield updated

  def handleLoaded(loaded: SetGraph): IO[Unit] = {
    val graphml = loaded.graphml

    for {
      oldPage <- page.get
      newPage <- createPage
      _ <- page.set(newPage)
      _ <- setNewPage(newPage, oldPage)
      rendered <- renderGraphML(graphml)
    } yield rendered
  }

  def renderGraphML(graphml: GraphMLGraph[Json, Json]): IO[Unit] = for {
    _ <- graphml.graph.nodes.map(appendNode(graphml)(_)).toList.sequence
    _ <- graphml.graph.edges.map(appendEdge(graphml)(_)).toList.sequence
  } yield ()

  def handleEvents: Eval = ctx => ctx.effect(this) {
    case ResetTransformation => handleResetTransformation

    case loaded: SetGraph => handleLoaded(loaded).void

    case setModel: SetModel => handleSetModel(setModel)

    case selected: Selected => updateSelections(selected.oldSelection, selected.elements)

    case ElementUpdated(element, Deleted, _) => deleteElementRef(element)

    case ElementUpdated(ElementRef(id, EdgeType), _, _) => updateEdge(id, ctx).void
    case ElementUpdated(ElementRef(id, NodeType), _, _) => updateNode(id, ctx).void
  }.flatMap(handleCreateNode)

  def handleResetTransformation: IO[Unit] = for {
    p <- page.get
    _ <- p.resetTransformation
  } yield ()

  def handleSetModel(setModel: SetModel): IO[Unit] = for {
    vm <- viewModel.get
    _ <- vm.graphElements.keys.map(deleteElementRef).toList.sequence
    _ <- renderGraphML(setModel.model.graphml)
  } yield ()

  override lazy val eval: Eval = handleEvents

  def handleCreateNode: Eval = ctx => ctx.transformIO {
    case create: CreateNode => for {
      p <- page.get
      center = p.pageCenter
    } yield {
      ctx
        .copy(event = create.copy(x = create.x.orElse(Some(center.x)), y = create.y.orElse(Some(center.y))))
    }
  }

  def updateSelections(oldSelections: Set[ElementRef], newSelections: Set[ElementRef]): IO[Unit] =
    for {
      vm <- viewModel.get
      p <- page.get
      _ <- IO {
        oldSelections.foreach(elem => vm.graphElements.get(elem).foreach(p.unsetSelection(_).unsafeRunSync()))
        newSelections.foreach(elem => vm.graphElements.get(elem).foreach(p.setSelection(_).unsafeRunSync()))
      }
    } yield ()

  def setNewPage(newPage: Page[T], oldPage: Page[T]): IO[Unit]

  def deleteElementRef(elementRef: ElementRef): IO[Unit] = for {
    vm <- viewModel.get
    p <- page.get
    _ <- vm.graphElements.get(elementRef).map(element => p.deleteElement(element)).getOrElse(IO.unit)
    _ <- viewModel.update(vm => vm.copy(graphElements = vm.graphElements - elementRef))
  } yield ()
}
