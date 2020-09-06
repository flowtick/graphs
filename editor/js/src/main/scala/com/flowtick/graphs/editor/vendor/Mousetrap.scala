package com.flowtick.graphs.editor.vendor

import org.scalajs.dom.Event

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

@js.native
@JSGlobal("Mousetrap")
object Mousetrap extends js.Any {
  def bind(sequence: String, handler: js.Function1[Event, Unit]): Unit = js.native
}
