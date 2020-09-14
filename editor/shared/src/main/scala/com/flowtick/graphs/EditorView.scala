package com.flowtick.graphs

import cats.effect.IO
import cats.effect.concurrent.Ref
import cats.implicits._
import com.flowtick.graphs.graphml.{GraphMLEdge, GraphMLGraph, GraphMLNode, PointSpec}
import io.circe.Json

final case class DragContext(mouseAnchorX: Double, mouseAnchorY: Double, translateAnchorX: Double, translateAnchorY: Double)

final case class DragStart[T](cursorX: Double,
                              cursorY: Double,
                              transformX: Double,
                              transformY: Double,
                              dragElem: T,
                              element: ElementRef,
                              lastPos: Option[(Double, Double)])

trait Page[T] {
  def root: T
  def pageCenter: PointSpec

  def addEdge(edge: Edge[GraphMLEdge[Json]],
              graphml: GraphMLGraph[Json, Json]): IO[Option[GraphElement[T]]]

  def addNode(node: Node[GraphMLNode[Json]], graphml: GraphMLGraph[Json, Json]): IO[Option[GraphElement[T]]]

  def setSelection(element: GraphElement[T]): IO[Unit]

  def unsetSelection(element: GraphElement[T]): IO[Unit]

  def deleteElement(element: GraphElement[T]): IO[Unit]

  def reset: IO[Unit]
}

trait GraphElement[+T] {
  def id: String
  def group: T
  def selectElem: T
  def label: T
}

final case class ViewModel[T](graphElements: Map[String, GraphElement[T]])

trait EditorView[T] extends EditorComponent {
  lazy val page = Ref.unsafe[IO, Page[T]](createPage.unsafeRunSync())
  lazy val viewModel: Ref[IO, ViewModel[T]] = Ref.unsafe(ViewModel(Map.empty))

  def createPage: IO[Page[T]]

  def messageBus: EditorMessageBus

  def handleSelect(element: ElementRef): IO[Unit] = for {
    vm <- viewModel.get
    newSelections =
    if(vm.graphElements.contains(element.id)) List(element)
    else List.empty
    _ <- messageBus.notifyEvent(this, Select(newSelections))
  } yield ()

  def handleDoubleClick: Any => IO[Unit] = (_: Any) => {
    messageBus.notifyEvent(this, Toggle(Toggle.editKey, true)).void
  }

  def handleDrag(drag: Option[DragStart[T]]): IO[Unit] = (for {
    dragEvent <- drag
    (x, y) <- dragEvent.lastPos
  } yield messageBus.publish(Move(dragEvent.element, x, y)).void).getOrElse(IO.unit)

  def appendEdge(edge: Edge[GraphMLEdge[Json]], graphml: GraphMLGraph[Json, Json]): IO[Option[GraphElement[T]]] = for {
    page <- page.get

    edgeElement <- page.addEdge(edge, graphml)

    _ <- {
      def withEdge(vm: ViewModel[T]): ViewModel[T] = edgeElement match {
        case Some(elem) => vm.copy(graphElements = vm.graphElements + (edge.id -> elem))
        case None => vm
      }

      viewModel.update(withEdge)
    }
  } yield edgeElement

  def appendNode(node: Node[GraphMLNode[Json]], graphml: GraphMLGraph[Json, Json]): IO[Option[GraphElement[T]]] = for {
    pageElem <- page.get
    nodeElement <- pageElem.addNode(node, graphml)
    _ <- {
      def withNode(vm: ViewModel[T]): ViewModel[T] = nodeElement
        .map(elem => vm.copy(graphElements = vm.graphElements + (elem.id -> elem)))
        .getOrElse(vm)

      viewModel.update(withNode)
    }
  } yield nodeElement

  def updateNode(id: String, graphml: GraphMLGraph[Json, Json]): IO[Unit] = graphml.graph.findNode(id).map(node => for {
    vm <- viewModel.get
    p <- page.get
    _ <- vm.graphElements.get(node.id) match {
      case Some(element: GraphElement[T]) =>
        p.deleteElement(element) *> appendNode(node, graphml)

      case Some(other) =>
        IO.raiseError(new IllegalStateException(s"did not find node for id in view model, found: $other"))

      case None => appendNode(node, graphml)
    }
  } yield ()).getOrElse(IO.unit)

  def updateEdge(id: String, graphml: GraphMLGraph[Json, Json]): IO[Unit] = graphml.graph.findEdge(id).map(edge => for {
    vm <- viewModel.get
    p <- page.get
    _ <- vm.graphElements.get(edge.id) match {
      case Some(element: GraphElement[T]) =>
        p.deleteElement(element) *> appendEdge(edge, graphml)

      case Some(other) =>
        IO.raiseError(new IllegalStateException(s"did not find node for id in view model, found: $other"))

      case None => appendEdge(edge, graphml)
    }
  } yield ()).getOrElse(IO.unit)

  def handleLoaded(loaded: SetGraph): IO[Unit] = {
    val graphml = loaded.graphml

    for {
      oldPage <- page.get
      newPage <- createPage
      _ <- page.set(newPage)
      _ <- setNewPage(newPage, oldPage)
      _ <- graphml.graph.edges.map(appendEdge(_, graphml)).toList.sequence
      _ <- graphml.graph.nodes.map(appendNode(_, graphml)).toList.sequence
    } yield ()
  }

  def handleEvents: Eval = ctx => ctx.effect(this) {
    case Reset => handleReset

    case loaded: SetGraph => handleLoaded(loaded).void

    case selected: Selected => updateSelections(selected.oldSelection, selected.elements)

    case ElementUpdated(element, Deleted) => deleteElementRef(element)

    case ElementUpdated(ElementRef(id, EdgeType), _) => updateEdge(id, ctx.model.graphml)
    case ElementUpdated(ElementRef(id, NodeType), _) => updateNode(id, ctx.model.graphml)
  }.flatMap(handleCreateNode).flatMap(_.effect(this) {
    case _ => updateSelections(ctx.model.selection, ctx.model.selection)
  })

  def handleReset: IO[Unit] = for {
    p <- page.get
    _ <- p.reset
  } yield ()

  override lazy val eval: Eval = handleEvents

  def handleCreateNode: Eval = ctx => ctx.transformIO {
    case create: CreateNode => for {
      p <- page.get
      center = p.pageCenter
    } yield {
      ctx.copy(event = create.copy(x = create.x.orElse(Some(center.x)), y = create.y.orElse(Some(center.y))))
    }
  }

  def updateSelections(oldSelections: Seq[ElementRef], newSelections: Seq[ElementRef]): IO[Unit] =
    for {
      vm <- viewModel.get
      p <- page.get
      _ <- IO {
        oldSelections.foreach(elem => vm.graphElements.get(elem.id).foreach(p.unsetSelection(_).unsafeRunSync()))
        newSelections.foreach(elem => vm.graphElements.get(elem.id).foreach(p.setSelection(_).unsafeRunSync()))
      }
    } yield ()

  def setNewPage(newPage: Page[T], oldPage: Page[T]): IO[Unit]

  def deleteElementRef(elementRef: ElementRef): IO[Unit] = for {
    vm <- viewModel.get
    p <- page.get
    _ <- vm.graphElements.get(elementRef.id).map(p.deleteElement).getOrElse(IO.unit)
    _ <- viewModel.update(vm => vm.copy(graphElements = vm.graphElements - elementRef.id))
  } yield ()
}
