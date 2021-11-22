package com.flowtick.graphs.editor.vendor

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

@js.native
trait AceSession extends js.Any {
  def setMode(mode: String): Unit = js.native
  def setValue(value: String): Unit = js.native
  def on(
      event: String,
      handler: js.Function1[js.UndefOr[js.Object], Unit]
  ): Unit = js.native
}

@js.native
trait AceConfig extends js.Any {
  def set(key: String, value: String): js.native
}

@js.native
trait AceEditor extends js.Any {
  def setTheme(theme: String): Unit = js.native
  def session: AceSession = js.native
  def on(
      event: String,
      handler: js.Function1[js.UndefOr[js.Object], Unit]
  ): Unit = js.native
  def getValue(): String = js.native
}

@js.native
@JSGlobal("ace")
object Ace extends js.Any {
  def edit(element: js.Any): AceEditor = js.native
  def config: AceConfig = js.native
}
