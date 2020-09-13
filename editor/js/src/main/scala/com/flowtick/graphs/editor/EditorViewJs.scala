package com.flowtick.graphs.editor

import cats.effect.IO
import cats.implicits._
import com.flowtick.graphs.graphml.{GraphMLEdge, GraphMLGraph, GraphMLNode}
import com.flowtick.graphs.{Node, _}
import io.circe.Json
import org.scalajs.dom.raw.{MouseEvent, SVGElement}

class EditorViewJs(containerElementId: String)(val messageBus: EditorMessageBus) extends EditorView[SVGElement] {

  lazy val container = org.scalajs.dom.window.document.getElementById(containerElementId)

  def createPage: Page[SVGElement] = {
    val page = SVGPage()

    page.panZoomRect.onmousedown = e => page.startPan(e)
    page.panZoomRect.onmouseup = e => page.stopPan(e)
    page.panZoomRect.onmousemove = e => page.pan(e)

    page.root.addEventListener("wheel", page.zoom _)

    page.root.addEventListener("mousedown", (e: MouseEvent) => {
      page
        .click(e)
        .orElse(page.startDrag(e).map(_.element)) match {
        case Some(elementRef) => handleSelect(elementRef).unsafeRunSync()
        case None =>
      }
    })

    page.root.addEventListener("mousemove", page.drag)
    page.root.addEventListener("mouseup", (e: MouseEvent) => handleDrag(page.endDrag(e)))
    page.root.addEventListener("mouseleave", (e: MouseEvent) => {
      page.stopPan(e)
      handleDrag(page.endDrag(e))
    })

    page.root.ondblclick = _ => {
      messageBus
        .notifyEvent(this, Toggle(Toggle.editKey, true))
        .unsafeRunSync()
    }

    page
  }

  def updateSelections(oldSelections: Seq[ElementRef], newSelections: Seq[ElementRef]): IO[Unit] =
    for {
      vm <- viewModel.get
      _ <- IO {
        oldSelections.foreach(elem => vm.graphElements.get(elem.id).foreach(SVGGraphRenderer.unsetSelection))
        newSelections.foreach(elem => vm.graphElements.get(elem.id).foreach(SVGGraphRenderer.setSelection))
      }
    } yield ()

  def handleDrag(drag: Option[DragStart]): Unit = for {
    dragEvent <- drag
    (x, y) <- dragEvent.lastPos
  } yield messageBus
    .publish(Move(dragEvent.element, x, y))
    .unsafeRunSync()

  override def init(model: EditorModel): IO[Unit] = page.get.map(p => container.insertBefore(p.root, container.firstChild))

  def handleCreateNode: Eval = ctx => ctx.transformIO {
    case create: CreateNode => for {
      p <- page.get
      center = p.pageCenter
    } yield {
      ctx.copy(event = create.copy(x = create.x.orElse(Some(center.x)), y = create.y.orElse(Some(center.y))))
    }
  }

  def handleEvents: Eval = ctx => ctx.effect(this) {
    case loaded: SetGraph => handleLoaded(loaded).void

    case selected: Selected => updateSelections(selected.oldSelection, selected.elements)

    case ElementUpdated(element, Deleted) => deleteElementRef(element)

    case ElementUpdated(ElementRef(id, EdgeType), _) => updateEdge(id, ctx.model.graphml)
    case ElementUpdated(ElementRef(id, NodeType), _) => updateNode(id, ctx.model.graphml)
  }.flatMap(handleCreateNode).flatMap(_.effect(this) {
    case _ => updateSelections(ctx.model.selection, ctx.model.selection)
  })

  override lazy val eval: Eval = handleEvents

  def handleLoaded(loaded: SetGraph): IO[Unit] = {
    val newPage = createPage
    val graphml = loaded.graphml

    for {
      oldPage <- page.getAndSet(newPage)
      _ <- IO(container.replaceChild(newPage.root, oldPage.root))

      _ <- graphml.graph.edges.map(appendEdge(_, graphml)).toList.sequence
      _ <- graphml.graph.nodes.map(appendNode(_, graphml)).toList.sequence
    } yield ()
  }

  def updateEdge(id: String, graphml: GraphMLGraph[Json, Json]): IO[Unit] = graphml.graph.findEdge(id).map(edge => for {
    vm <- viewModel.get
    _ <- vm.graphElements.get(edge.id) match {
      case Some(element: EdgeElement) =>
        deleteElement(element) *> appendEdge(edge, graphml)

      case Some(other) =>
        IO.raiseError(new IllegalStateException(s"did not find node for id in view model, found: $other"))

      case None => appendEdge(edge, graphml)
    }
  } yield ()).getOrElse(IO.unit)

  def deleteElementRef(elementRef: ElementRef): IO[Unit] = for {
    vm <- viewModel.get
    _ <- vm.graphElements.get(elementRef.id).map(deleteElement).getOrElse(IO.unit)
  } yield ()

  def deleteElement(element: GraphElement[SVGElement]): IO[Unit] = for {
    _ <- IO(element.group.parentNode.removeChild(element.group))
    _ <- IO(element.selectElem.parentNode.removeChild(element.selectElem)).attempt.void
    _ <- IO(element.label.parentNode.removeChild(element.label)).attempt.void
    _ <- viewModel.update(vm => vm.copy(graphElements = vm.graphElements - element.id))
  } yield ()

  def appendEdge(edge: Edge[GraphMLEdge[Json]], graphml: GraphMLGraph[Json, Json]): IO[Option[GraphElement[SVGElement]]] = for {
    page <- page.get

    edgeElement <- page.addEdge(edge, graphml)

    _ <- {
      def withEdge(vm: ViewModel[SVGElement]): ViewModel[SVGElement] = edgeElement match {
        case Some(elem) => vm.copy(graphElements = vm.graphElements + (edge.id -> elem))
        case None => vm
      }

      viewModel.update(withEdge)
    }
  } yield edgeElement

  def updateNode(id: String, graphml: GraphMLGraph[Json, Json]): IO[Unit] = graphml.graph.findNode(id).map(node => for {
      vm <- viewModel.get
      _ <- vm.graphElements.get(node.id) match {
        case Some(element: NodeElement) =>
          deleteElement(element) *> appendNode(node, graphml)

        case Some(other) =>
          IO.raiseError(new IllegalStateException(s"did not find node for id in view model, found: $other"))

        case None => appendNode(node, graphml)
      }
  } yield ()).getOrElse(IO.unit)

  def appendNode(node: Node[GraphMLNode[Json]], graphml: GraphMLGraph[Json, Json]): IO[Option[GraphElement[SVGElement]]] = for {
    pageElem <- page.get
    nodeElement <- pageElem.addNode(node, graphml)
    _ <- {
      def withNode(vm: ViewModel[SVGElement]): ViewModel[SVGElement] = nodeElement
        .map(elem => vm.copy(graphElements = vm.graphElements + (elem.id -> elem)))
        .getOrElse(vm)

      viewModel.update(withNode)
    }
  } yield nodeElement
}
