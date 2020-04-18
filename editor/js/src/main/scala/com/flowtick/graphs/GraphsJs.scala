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
    graph: Graph[JsGraph, JsEdge, JsNode]): js.UndefOr[MxGraph] = {
    val container = org.scalajs.dom.window.document.getElementById(containerElementId)

    MxGraphView.create(container, graph)(
      implicitly[Identifiable[JsNode, String]],
      Labeled.label[Edge[JsEdge, JsNode], Option[String]](edge => edge.value.label))
  }

  @JSExport
  def toGraph(mxGraph: MxGraph): Graph[JsGraph, JsEdge, JsNode] =
    MxGraphView.toGraph(JsGraph(None), mxGraph)

  def main(args: Array[String]): Unit = {
    println("graphs loaded...")
  }

}
