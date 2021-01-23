package com.flowtick.graphs.editor.vendor

import org.scalajs.dom.raw.{SVGElement, SVGMatrix}

object SVGUtil {
  def setMatrix(elem: SVGElement, matrix: SVGMatrix): Unit =
    setTransform(elem, "matrix(" + matrix.a + "," +
      matrix.b + "," +
      matrix.c + "," +
      matrix.d + "," +
      matrix.e + "," +
      matrix.f + ")")

  def setTransform(elem: SVGElement, transform: String): Unit = {
    elem.setAttributeNS(
      null,
      "transform",
      transform
    )
  }
}
