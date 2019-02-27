package com.flowtick.graphs

import com.flowtick.graphs.JsGraph._
import com.flowtick.graphs.defaults._
import com.flowtick.graphs.defaults.directed._
import mxgraph.{ MxGraph, MxGraphView }

import scala.scalajs.js
import scala.scalajs.js.annotation.{ JSExport, JSExportTopLevel }

@JSExportTopLevel("graphs")
object GraphsJs {

  @JSExport
  def createView(
    containerElementId: String,
    graph: DefaultGraph[Edge[JsEdge, JsNode], JsNode, JsGraph]): js.UndefOr[MxGraph] = {
    val container = org.scalajs.dom.window.document.getElementById(containerElementId)

    MxGraphView.create(container, graph)(
      implicitly[Graph[DefaultGraph, Edge]],
      implicitly[EdgeType[Edge]],
      implicitly[Identifiable[JsNode]],
      implicitly[Labeled[Edge[JsEdge, JsNode], String]])
  }

  @JSExport
  def toGraph(mxGraph: MxGraph): DefaultGraph[Edge[JsEdge, JsNode], JsNode, JsGraph] =
    MxGraphView.toGraph[DefaultGraph, Edge](JsGraph(None), mxGraph)

  @JSExport
  def exampleGraph: DefaultGraph[Edge[JsEdge, JsNode], JsNode, JsGraph] = defaultGraph.withValue(JsGraph(Some("example")))(Seq(
    n(JsNode("A")) --> (JsEdge(None, None), n(JsNode("B")))))

  def main(args: Array[String]): Unit = {
    println("graphs loaded...")
  }

}
