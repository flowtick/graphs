package com.flowtick.graphs

import cats.effect.IO
import cats.effect.concurrent.Ref
import com.flowtick.graphs.graphml.{GraphMLEdge, GraphMLGraph, GraphMLNode, PointSpec}
import io.circe.Json

trait Page[+T] {
  def root: T
  def pageCenter: PointSpec

  def addEdge(edge: Edge[GraphMLEdge[Json]],
              graphml: GraphMLGraph[Json, Json]): IO[Option[GraphElement[T]]]

  def addNode(node: Node[GraphMLNode[Json]], graphml: GraphMLGraph[Json, Json]): IO[Option[GraphElement[T]]]
}

trait GraphElement[+T] {
  def id: String
  def group: T
  def selectElem: T
  def label: T
}

final case class ViewModel[+T](graphElements: Map[String, GraphElement[T]])


trait EditorView[T] extends EditorComponent {
  lazy val page = Ref.unsafe[IO, Page[T]](createPage)
  lazy val viewModel: Ref[IO, ViewModel[T]] = Ref.unsafe(ViewModel(Map.empty))

  def createPage: Page[T]

  def messageBus: EditorMessageBus

  def handleSelect(element: ElementRef): IO[Unit] = for {
    vm <- viewModel.get
    newSelections =
    if(vm.graphElements.contains(element.id)) List(element)
    else List.empty
    _ <- messageBus.notifyEvent(this, Select(newSelections))
  } yield ()

}
