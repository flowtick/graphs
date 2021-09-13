package com.flowtick.graphs.layout

import scala.scalajs.js
import scala.scalajs.js.{Promise, UndefOr}
import scala.scalajs.js.annotation.{JSExportTopLevel, JSGlobal, JSImport}

@JSExportTopLevel("ElkLayoutNode")
class ElkLayoutNode(val id: String, val x: Double = 0.0, val y: Double = 0.0, val width: Double, val height: Double, val children: js.Array[ElkLayoutNode] = js.Array.apply()) extends js.Object

@JSExportTopLevel("ElkLayoutPoint")
class ElkLayoutPoint(val x: Double, val y: Double) extends js.Object

@JSExportTopLevel("ElkLayoutEdgeSection")
class ElkLayoutEdgeSection(val id: String,
                           val incomingShape: String,
                           val outgoingShape: String,
                           val startPoint: ElkLayoutPoint,
                           val endPoint: ElkLayoutPoint,
                           val bendPoints: js.UndefOr[js.Array[ElkLayoutPoint]]) extends js.Object

@JSExportTopLevel("ElkLayoutEdge")
class ElkLayoutEdge(val id: String,
                    val sources: js.Array[String] = js.Array.apply(),
                    val targets: js.Array[String] = js.Array.apply(),
                    val sections: js.Array[ElkLayoutEdgeSection] = js.Array.apply()) extends js.Object

@JSExportTopLevel("ElkLayoutGraph")
class ElkLayoutGraph(val id: String,
                     val layoutOptions:
                     js.Dictionary[js.Any] = js.Dictionary.apply(
                      "elk.algorithm" -> "layered"
                     ),
                     val children: js.Array[ElkLayoutNode] = js.Array(),
                     val edges: js.Array[ElkLayoutEdge] = js.Array()) extends js.Object {
  var width: UndefOr[Double] = 0.0
  var height: UndefOr[Double] = 0.0
}

@js.native
trait ELKJS extends js.Any {
  def layout(graph: ElkLayoutGraph): Promise[ElkLayoutGraph] = js.native
}

@JSImport("elkjs", JSImport.Namespace)
@js.native
class ELKImport extends ELKJS

@JSGlobal("ELK")
@js.native
class ELKGlobal extends ELKJS

