package com.flowtick.graphs.editor

import com.flowtick.graphs.{Edge, Graph, Identifiable, Labeled}

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import com.flowtick.graphs.defaults._
import com.flowtick.graphs.defaults.directed._
import com.flowtick.graphs.layout.GraphLayout
import com.flowtick.graphs.view.mxgraph._

@JSExportTopLevel("graphsEditor")
object GraphsEditor {

  @JSExport
  def showDefaultGraph(
    containerElementId: String,
    graph: Graph[DefaultNode, DirectedEdge[DefaultNode]]): Unit = {
    val container = org.scalajs.dom.window.document.getElementById(containerElementId)

    new MxGraphView().createView(container, GraphLayout.none, graph)(
      implicitly[Identifiable[DefaultNode]],
      implicitly[Labeled[DirectedEdge[DefaultNode], String]],
    )
  }

  @JSExport
  def exampleGraph: Graph[DefaultNode, DirectedEdge[DefaultNode]] = DefaultGraph.create[DefaultNode](Seq(
    n("A") -> n("B")))

  def main(args: Array[String]): Unit = {
    println("graphs loaded...")
  }

}
