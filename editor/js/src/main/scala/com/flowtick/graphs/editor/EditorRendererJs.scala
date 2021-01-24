package com.flowtick.graphs.editor

import cats.effect.IO
import com.flowtick.graphs.editor.vendor.SVGUtil
import com.flowtick.graphs.editor.view._
import org.scalajs.dom
import org.scalajs.dom.raw._
import org.scalajs.dom.svg.{G, SVG}
import scalatags.JsDom
import scalatags.JsDom.all._

object EditorRendererJs {
  private implicit val domSVGMatrix: SVGMatrixLike[SVGMatrix] = new SVGMatrixLike[SVGMatrix] {
    private val matrixSvg = JsDom.svgTags.svg.render // unattached svg for independent calculations

    override def inverse(matrix: SVGMatrix): SVGMatrix = matrix.inverse()

    override def tx(matrix: SVGMatrix): Double = matrix.e
    override def tx_=(matrix: SVGMatrix)(value: Double): Unit = matrix.e = value

    override def ty(matrix: SVGMatrix): Double = matrix.f
    override def ty_=(matrix: SVGMatrix)(value: Double): Unit = matrix.f = value

    override def scalex(matrix: SVGMatrix): Double = matrix.a
    override def scalex_=(matrix: SVGMatrix)(value: Double): Unit = matrix.a = value

    override def scaley(matrix: SVGMatrix): Double = matrix.d
    override def scaley_=(matrix: SVGMatrix)(value: Double): Unit = matrix.d = value

    override def translate(matrix: SVGMatrix)(dx: Double, dy: Double): SVGMatrix = matrix.translate(dx, dy)
    override def scale(matrix: SVGMatrix)(factor: Double): SVGMatrix = matrix.scale(factor)

    override def transformPoint(matrix: SVGMatrix)(x: Double, y: Double): PagePoint = {
      val point = matrixSvg.createSVGPoint()
      point.x = x
      point.y = y
      val transformed = point.matrixTransform(matrix)
      PagePoint(transformed.x, transformed.y)
    }

    override def identity: SVGMatrix = matrixSvg.createSVGMatrix()
  }

  val renderer: SVGRenderer[Element, Element, Node, SVGMatrix] = new SVGRenderer[dom.Element, dom.Element, dom.Node, SVGMatrix](JsDom) {
    private val svgElem: SVG = graphSVG.root.asInstanceOf[SVG]
    private val viewPort: G = graphSVG.viewPort.asInstanceOf[G]

    private var pageMatrix: SVGMatrix = svgElem.createSVGMatrix()

    private val draggableClass = "draggable"
    private val selectableClass = "selectable"

    override def parseSvg(svgXml: String): dom.Element = {
      new DOMParser()
        .parseFromString(svgXml, "application/xml")
        .firstChild
        .asInstanceOf[dom.Element]
    }

    override protected def getScreenCTM: SVGMatrix = svgElem.getScreenCTM()
    override protected def getPageMatrix: SVGMatrix = pageMatrix

    override def applyTransformation(transformation: SVGMatrix): Unit = {
      pageMatrix = transformation
      SVGUtil.setMatrix(viewPort, pageMatrix)
    }

    override def x(elem: dom.Element): Double = elem.asInstanceOf[SVGLocatable].getCTM().e
    override def y(elem: dom.Element): Double = elem.asInstanceOf[SVGLocatable].getCTM().f

    override def setPosition(elem: dom.Element)(x: Double, y: Double): Unit =
      SVGUtil.setTransform(elem.asInstanceOf[SVGElement], s"translate($x $y)")

    override def selectElement(graphElement: GraphElement[Element]): IO[Unit] = IO {
      graphElement.selectElem.foreach(_.setAttribute("stroke", "#555555"))
      graphElement.selectElem.foreach(_.setAttribute("stroke-dasharray", "3 5"))
      graphElement.selectElem.foreach(_.classList.add(draggableClass))
    }

    override def unselectElement(graphElement: GraphElement[Element]): IO[Unit] = IO {
      graphElement.selectElem.foreach(_.setAttribute("stroke", null))
      graphElement.selectElem.foreach(_.setAttribute("stroke-dasharray", null))
      graphElement.selectElem.foreach(_.classList.remove(draggableClass))
    }

    override def selectable(elem: Element): Option[ElementRef] = {
      if (elem.classList.contains(selectableClass)) {
        Some(referenceFromElement(elem))
      } else None
    }

    override def draggable(elem: Element): Option[ElementRef] = {
      if (elem.classList.contains(draggableClass)) {
        Some(referenceFromElement(elem))
      } else None
    }

    private def referenceFromElement(elem: Element): ElementRef = {
      val elementType = elem.getAttribute("data-type")
      val elementId = elem.getAttribute("data-id")

      val refType = elementType match {
        case "node" => NodeType
        case "edge" => EdgeType
      }

      ElementRef(elementId, refType)
    }

    override def deleteElement(element: GraphElement[Element]): IO[Unit] = for {
      _ <- IO(element.group.parentNode.removeChild(element.group)).attempt.void
      _ <- IO(element.selectElem.foreach(selectElem => selectElem.parentNode.removeChild(selectElem))).attempt.void
      _ <- IO(element.label.parentNode.removeChild(element.label)).attempt.void
    } yield ()

    override def appendChild(element: Element)(child: Element): Unit =
      element.appendChild(child)
  }
}