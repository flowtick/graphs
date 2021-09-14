package com.flowtick.graphs.editor.view

import cats.effect.IO
import com.flowtick.graphs.editor.{ElementRef, PagePoint}
import javafx.scene.transform.Affine
import org.apache.batik.anim.dom.SAXSVGDocumentFactory
import org.apache.batik.util.{SVGConstants, XMLResourceDescriptor}
import org.w3c.dom.svg.SVGDocument
import org.w3c.dom.{Document, Element, Node}
import scalatags.vdom.Builder
import scalatags.{VirtualDom, generic, vdom}

import java.io.ByteArrayInputStream
import scala.util.Try

class XmlDom(val svgDocument: SVGDocument) extends VirtualDom[Element, Node] {
  override def stringToFrag(s: String): Node = {
    svgDocument.createTextNode(s)
  }

  override def rawToFrag(s: String): Node =
    svgDocument.createTextNode(s)

  class ElemBuilder(elem: Element) extends vdom.Builder[Element, Node] {
    override def appendChild(child: Node): Unit = elem.appendChild(child)
    override def appendClass(cls: String): Unit = ()
    override def appendStyle(cssName: String, value: String): Unit = ()
    override def setAttr(name: String, value: String): Unit = elem.setAttribute(name, value)
    override def render(): Element = elem
  }

  override def makeBuilder(tag: String): vdom.Builder[Element, Node] = {
    val newElement = svgDocument.createElementNS(SVGConstants.SVG_NAMESPACE_URI, tag)
    val newBuilder = new ElemBuilder(newElement)
    tag match {
      case "svg" if svgDocument.getRootElement.getChildNodes.getLength == 0 =>
        svgDocument.removeChild(svgDocument.getRootElement)
        svgDocument.appendChild(newElement)
        newBuilder
      case _ => newBuilder
    }
  }
}

object XmlDom {
  private val factory = new SAXSVGDocumentFactory(XMLResourceDescriptor.getXMLParserClassName)

  def newSvgDocument: SVGDocument =
    parseSvg(s"""<svg xmlns="${SVGConstants.SVG_NAMESPACE_URI}" />""")

  def parseSvg(svgXml: String): SVGDocument = this.synchronized {
    factory.createSVGDocument(null, new ByteArrayInputStream(svgXml.getBytes("UTF-8")))
  }

  import java.io.StringWriter
  import javax.xml.transform.TransformerFactory
  import javax.xml.transform.dom.DOMSource
  import javax.xml.transform.stream.StreamResult

  def toString(document: Document): Try[String] =
    Try {
      val tf = TransformerFactory.newInstance
      val trans = tf.newTransformer
      val sw = new StringWriter
      trans.transform(new DOMSource(document), new StreamResult(sw))
      sw.toString
    }
}

class SVGRendererJvm(options: SVGRendererOptions)(implicit appender: Element => generic.Frag[vdom.Builder[Element, Node], Node], matrixLike: SVGMatrixLike[Affine], xmlDom: XmlDom) extends SVGRenderer(xmlDom, options) {
  override def parseSvg(svgXml: String): Element = XmlDom.parseSvg(svgXml).getRootElement

  override protected val getPageMatrix: Affine = new Affine()
  override protected def getScreenCTM: Affine = new Affine()
  override protected def applyTransformation(transformation: Affine): Unit = {
    getPageMatrix.setToTransform(transformation)
    graphSVG.viewPort.setAttribute("transform", s"matrix(${transformation.getMxx} 0 0 ${transformation.getMyy} ${transformation.getTx} ${transformation.getTy})")
    Try(setDimensions(graphSVG.root.getAttribute("width").toInt, graphSVG.root.getAttribute("height").toInt))
  }

  override def x(elem: Element): Double = 0.0
  override def y(elem: Element): Double = 0.0

  override def setPosition(elem: Element)(x: Double, y: Double): Unit = ()

  override def selectElement(value: GraphElement[Element]): IO[Unit] = IO.unit
  override def unselectElement(value: GraphElement[Element]): IO[Unit] = IO.unit
  override def deleteElement(element: GraphElement[Element]): IO[Unit] = IO.unit

  override def selectable(elem: Element): Option[ElementRef] = None
  override def draggable(elem: Element): Option[ElementRef] = None

  override def appendChild(elem: Element)(child: Element): Unit =
    elem.appendChild(child)

  override protected def renderSelectRect(elementId: String, elementType: String, x: Double, y: Double, width: Double, height: Double): Option[Element] = None
  override protected def renderPanZoomRect: Option[Element] = None

  def toXmlString: Try[String] = XmlDom.toString(xmlDom.svgDocument)

  override def setDimensions(width: Double, height: Double): Unit = {
    val padding = options.padding.getOrElse(0.0)
    val newWidth = (matrixLike.scalex(getPageMatrix) * width)
    val newHeight = (matrixLike.scaley(getPageMatrix) * height)

    graphSVG.root.setAttribute("viewBox", s"${-padding / 2.0} ${-padding / 2.0} ${newWidth + padding} ${newHeight + padding}")
    graphSVG.root.setAttribute("width", newWidth.toString)
    graphSVG.root.setAttribute("height", newHeight.toString)
  }

  override lazy val graphSVG: GraphSVG[Element] = renderRootSvg()
}

object SVGRendererJvm {
  private implicit val appender: Element => generic.Frag[vdom.Builder[Element, Node], Node] = elem => new generic.Frag[vdom.Builder[Element, Node], Node] {
    override def render: Node = elem
    override def applyTo(t: Builder[Element, Node]): Unit = t.appendChild(elem)
  }

  private implicit val matrixLike: SVGMatrixLike[Affine] = new SVGMatrixLike[Affine] {
    override def identity: Affine = new Affine()
    override def inverse(matrix: Affine): Affine = matrix.createInverse()

    override def tx(matrix: Affine): Double = matrix.getTx
    override def tx_=(matrix: Affine)(value: Double): Unit = matrix.setTx(value)

    override def ty(matrix: Affine): Double = matrix.getTy
    override def ty_=(matrix: Affine)(value: Double): Unit = matrix.setTy(value)

    override def scalex(matrix: Affine): Double = matrix.getMxx
    override def scalex_=(matrix: Affine)(value: Double): Unit = matrix.setMxx(value)

    override def scaley(matrix: Affine): Double = matrix.getMyy
    override def scaley_=(matrix: Affine)(value: Double): Unit = matrix.setMyy(value)

    override def translate(matrix: Affine)(dx: Double, dy: Double): Affine = {
      matrix.appendTranslation(dx, dy)
      matrix
    }

    override def scale(matrix: Affine)(factor: Double): Affine = {
      matrix.appendScale(factor, factor)
      matrix
    }

    override def transformPoint(matrix: Affine)(x: Double, y: Double): PagePoint = {
      val transformed = matrix.transform(x, y)
      PagePoint(transformed.getX, transformed.getY)
    }
  }

  private implicit def xmlDomInstance: XmlDom = new XmlDom(XmlDom.newSvgDocument)

  def apply(options: SVGRendererOptions = SVGRendererOptions()): SVGRendererJvm = new SVGRendererJvm(options)
}
