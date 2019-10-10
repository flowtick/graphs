package com.flowtick.graphs

import com.flowtick.graphs.JsGraph._
import com.flowtick.graphs.defaults._
import mxgraph.{ MxGraph, MxGraphView }

import scala.scalajs.js
import scala.scalajs.js.annotation.{ JSExport, JSExportTopLevel }

@JSExportTopLevel("graphs")
object GraphsJs {

  @JSExport
  def createView(
    containerElementId: String,
    graph: Graph[JsEdge, JsNode, JsGraph]): js.UndefOr[MxGraph] = {
    val container = org.scalajs.dom.window.document.getElementById(containerElementId)

    MxGraphView.create(container, graph)(
      implicitly[Identifiable[JsNode]],
      implicitly[Labeled[Edge[JsEdge, JsNode], String]])
  }

  @JSExport
  def toGraph(mxGraph: MxGraph): Graph[JsEdge, JsNode, JsGraph] =
    MxGraphView.toGraph(JsGraph(None), mxGraph)

  @JSExport
  def exampleGraph: Graph[JsEdge, JsNode, JsGraph] = Graph.from(JsGraph(Some("example")), edges = Seq(
    n(JsNode("A")) --> (JsEdge(None, None), n(JsNode("B")))))

  def main(args: Array[String]): Unit = {
    println("graphs loaded...")
  }

}
