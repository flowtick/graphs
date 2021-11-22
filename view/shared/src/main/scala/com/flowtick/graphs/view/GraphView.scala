package com.flowtick.graphs.view

import cats.effect.IO
import cats.implicits._
import com.flowtick.graphs.layout.{GraphLayout, GraphLayoutLike, PointSpec}
import com.flowtick.graphs.{Edge, Graph, Labeled, Node}
import cats.effect.kernel.Ref
import com.flowtick.graphs.style.{StyleRef, StyleSheet, StyleSheetLike}

sealed trait ElementType
case object NodeElementType extends ElementType
case object EdgeElementType extends ElementType

final case class ElementRef(id: String, elementType: ElementType)

trait GraphElement[+T] {
  def id: ElementRef
  def group: T
  def selectElem: Option[T]
  def label: T
}

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

trait Page[PageElement, PageEvent, NodeType, EdgeType, Model] {
  type Point = PagePoint

  var dragStartRef: Ref[IO, Option[DragStart[PageElement]]] = Ref.unsafe(None)

  def root: PageElement
  def pageCenter: PointSpec

  def addEdge(
      edge: Edge[EdgeType],
      editorModel: Model
  ): IO[Option[GraphElement[PageElement]]]

  def addNode(
      node: Node[NodeType],
      editorModel: Model
  ): IO[Option[GraphElement[PageElement]]]

  def setSelection(element: GraphElement[PageElement]): IO[Unit]

  def unsetSelection(element: GraphElement[PageElement]): IO[Unit]

  def deleteElement(element: GraphElement[PageElement]): IO[Unit]

  def resetTransformation: IO[Unit]

  def screenCoordinates(x: Double, y: Double): Point

  def pageCoordinates(x: Double, y: Double): Point

  def beforeDrag: PageEvent => IO[Unit]

  def eventCoordinates(event: PageEvent): PagePoint

  def applyDrag: DragStart[PageElement] => IO[Unit]

  def drag: PageEvent => IO[Unit] = evt =>
    for {
      currentDrag <- dragStartRef.get
      _ <- currentDrag match {
        case Some(dragStart) =>
          beforeDrag(evt) *> IO {
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

            dragStart.copy(
              lastPos = Some(PagePoint(tx, ty)),
              deltaX = deltaX,
              deltaY = deltaY
            )
          }.flatTap(applyDrag).map(Some.apply).flatMap(dragStartRef.set)
        case None => IO.pure(None)
      }
    } yield ()

  def endDrag: PageEvent => IO[Option[DragStart[PageElement]]] = _ =>
    dragStartRef.getAndUpdate(_ => None)
}

trait MessageBus[Component, Command, Event, Context] {
  def subscribe(backend: Component): IO[Component]
  def notifyEvent(
      source: Component,
      event: Event
  ): IO[Context]
  def publish(command: Command): IO[Context]
  def publishAll(commands: Vector[Command]): IO[Vector[Context]]
}

trait ViewContextLike[E, N] {
  def graph: Graph[E, N]
  def layout: GraphLayoutLike
  def styleSheet: StyleSheetLike

  def selection: Set[ElementRef]

  def nodeLabel: Labeled[N, String]
  def nodeStyleRef: StyleRef[Node[N]]
  def edgeLabel: Labeled[E, String]
  def edgeStyleRef: StyleRef[Edge[E]]
}

final case class ViewContext[E, N](
    graph: Graph[E, N],
    layout: GraphLayoutLike = GraphLayout(),
    styleSheet: StyleSheetLike = StyleSheet(),
    selection: Set[ElementRef] = Set.empty
)(implicit
    val nodeLabel: Labeled[N, String],
    val nodeStyleRef: StyleRef[Node[N]],
    val edgeLabel: Labeled[E, String],
    val edgeStyleRef: StyleRef[Edge[E]]
) extends ViewContextLike[E, N]

final case class ViewElements[+T](elements: Map[ElementRef, GraphElement[T]])

trait GraphView[PageElement, PageEvent, NodeType, EdgeType, Model <: ViewContextLike[
  EdgeType,
  NodeType
]] {
  type PageType = Page[PageElement, PageEvent, NodeType, EdgeType, Model]

  lazy val pageRef: Ref[IO, Option[PageType]] = Ref.unsafe(None)
  lazy val viewElements: Ref[IO, ViewElements[PageElement]] = Ref.unsafe(ViewElements(Map.empty))

  def createPage: IO[PageType]
  def handleSelect(element: ElementRef): Boolean => IO[Unit]
  def handleDoubleClick: Any => IO[Unit]
  def handleDrag(drag: Option[DragStart[PageElement]]): IO[Unit]

  protected def requirePage: IO[PageType] = pageRef.get.flatMap(
    IO.fromOption(_)(new IllegalStateException("page not set"))
  )

  def appendEdge(
      editorModel: Model
  )(edge: Edge[EdgeType]): IO[Option[GraphElement[PageElement]]] = for {
    page <- requirePage
    edgeElement <- page.addEdge(edge, editorModel)

    _ <- {
      def withEdge(view: ViewElements[PageElement]): ViewElements[PageElement] = edgeElement match {
        case Some(elem) =>
          view.copy(elements = view.elements + (elem.id -> elem))
        case None => view
      }

      viewElements.update(withEdge)
    }
  } yield edgeElement

  def appendNode(
      editorModel: Model
  )(node: Node[NodeType]): IO[Option[GraphElement[PageElement]]] = for {
    page <- requirePage
    nodeElement <- page.addNode(node, editorModel)
    _ <- {
      def withNode(view: ViewElements[PageElement]): ViewElements[PageElement] = nodeElement
        .map(elem => view.copy(elements = view.elements + (elem.id -> elem)))
        .getOrElse(view)

      viewElements.update(withNode)
    }
  } yield nodeElement

  def updateEdge(id: String, model: Model): IO[Option[GraphElement[PageElement]]] =
    model.graph
      .findEdge(id)
      .map(edge =>
        updateElement(edge, ElementRef(id, EdgeElementType), model)(
          appendEdge(model)
        )
      )
      .getOrElse(IO.pure(None))

  def updateNode(id: String, model: Model): IO[Option[GraphElement[PageElement]]] =
    model.graph
      .findNode(id)
      .map(edge =>
        updateElement(edge, ElementRef(id, NodeElementType), model)(
          appendNode(model)
        )
      )
      .getOrElse(IO.pure(None))

  def updateElement[EL](value: EL, id: ElementRef, model: Model)(
      update: EL => IO[Option[GraphElement[PageElement]]]
  ): IO[Option[GraphElement[PageElement]]] = for {
    view <- viewElements.get
    p <- requirePage
    updated <- view.elements.get(id) match {
      case Some(element: GraphElement[PageElement]) =>
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

  def createAndRender(model: Model): IO[Unit] =
    for {
      newPage <- createPage
      _ <- pageRef.set(Some(newPage))
      rendered <- renderGraph(model)
    } yield rendered

  def renderGraph(editorModel: Model): IO[Unit] = for {
    view <- viewElements.get
    _ <- view.elements.keys.map(deleteElementRef).toList.sequence
    _ <- editorModel.graph.nodes.map(appendNode(editorModel)(_)).toList.sequence
    _ <- editorModel.graph.edges.map(appendEdge(editorModel)(_)).toList.sequence
  } yield ()

  def handleResetTransformation: IO[Unit] = for {
    p <- requirePage
    _ <- p.resetTransformation
  } yield ()

  def updateSelections(
      oldSelections: Set[ElementRef],
      newSelections: Set[ElementRef]
  ): IO[Unit] =
    for {
      view <- viewElements.get
      p <- requirePage
      _ <- oldSelections.toList
        .flatMap(elem =>
          view.elements
            .get(elem)
            .map(p.unsetSelection)
        )
        .sequence
      _ <- newSelections.toList
        .flatMap(elem =>
          view.elements
            .get(elem)
            .map(p.setSelection)
        )
        .sequence
    } yield ()

  def deleteElementRef(elementRef: ElementRef): IO[Unit] = for {
    view <- viewElements.get
    p <- requirePage
    _ <- view.elements
      .get(elementRef)
      .map(element => p.deleteElement(element))
      .getOrElse(IO.unit)
    _ <- viewElements.update(currentView =>
      currentView.copy(elements = currentView.elements - elementRef)
    )
  } yield ()
}
