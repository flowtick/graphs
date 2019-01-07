package com.flowtick.graphs.view.mxgraph

import org.scalajs.dom.Element

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

@js.native
@JSGlobal(name = "mxGraph")
@SuppressWarnings(Array("org.wartremover.warts.Null"))
class MxGraph(container: Element) extends js.Any {
  def getModel(): GraphModel = js.native
  def getDefaultParent(): MxCell = js.native
  def getStylesheet(): MxStyleSheet = js.native

  def insertVertex(parent: MxCell, id: String, value: String, x: Int, y: Int, width: Int, height: Int, style: String = null): MxCell = js.native
  def insertEdge(parent: MxCell, id: String, value: String, source: MxCell, target: MxCell, style: String = null): MxCell = js.native
}

@js.native
trait MxCell extends js.Any {
}

@js.native
@JSGlobal(name = "mxEvent")
object MxEvent extends js.Any {
  def disableContextMenu(element: Element): Unit = js.native
}

@js.native
trait GraphModel extends js.Any {
  def beginUpdate(): Unit = js.native
  def endUpdate(): Unit = js.native
}

@js.native
@JSGlobal(name = "mxClient")
class MxClient extends js.Any

@js.native
@JSGlobal(name = "mxRubberband")
class MxRubberband(graph: MxGraph) extends js.Any

@js.native
@JSGlobal(name = "mxClient")
object MxClient extends js.Any {
  def isBrowserSupported(): Boolean = js.native
}

@js.native
@JSGlobal(name = "mxEvent")
class MxEvent extends js.Any

@js.native
@JSGlobal(name = "mxHierarchicalLayout")
@SuppressWarnings(Array("org.wartremover.warts.Var"))
class MxHierarchicalLayout(graph: MxGraph) extends js.Any {
  def execute(parent: MxCell): Unit = js.native
}

@js.native
@JSGlobal(name = "mxConstants")
object MxConstants extends js.Any {
  val STYLE_SHAPE: String = js.native // shape types: box
  val STYLE_ROUNDED: String = js.native // boolean
  val STYLE_SHADOW: String = js.native // boolean
  val STYLE_GRADIENTCOLOR: String = js.native // html color
  val STYLE_FILLCOLOR: String = js.native // html color
  val STYLE_STROKECOLOR: String = js.native // html color
  val STYLE_FONTCOLOR: String = js.native // hex

  val DIRECTION_NORTH: String = js.native // hex
  val DIRECTION_SOUTH: String = js.native // hex
  val DIRECTION_EAST: String = js.native // hex
  val DIRECTION_WEST: String = js.native // hex
}

@js.native
@JSGlobal(name = "mxStyleSheet")
class MxStyleSheet extends js.Any {
  def getDefaultVertexStyle(): js.Dictionary[js.Any] = js.native
  def getDefaultEdgeStyle(): js.Dictionary[js.Any] = js.native
}
