package com.flowtick.graphs.view

import cats.effect.IO
import org.apache.batik.anim.dom.SAXSVGDocumentFactory
import org.apache.batik.util.{SVGConstants, XMLResourceDescriptor}
import org.w3c.dom.svg.SVGDocument
import org.w3c.dom.{Document, Element, Node}
import scalatags.vdom.Builder
import scalatags.{VirtualDom, generic, vdom}

import java.awt.geom.{AffineTransform, Point2D}
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
    override def setAttr(name: String, value: String): Unit =
      elem.setAttribute(name, value)
    override def render(): Element = elem
  }

  override def makeBuilder(tag: String): vdom.Builder[Element, Node] = {
    val newElement =
      svgDocument.createElementNS(SVGConstants.SVG_NAMESPACE_URI, tag)
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
  private val factory = new SAXSVGDocumentFactory(
    XMLResourceDescriptor.getXMLParserClassName
  )

  def newSvgDocument: SVGDocument =
    parseSvg(s"""<svg xmlns="${SVGConstants.SVG_NAMESPACE_URI}" />""")

  def parseSvg(svgXml: String): SVGDocument = this.synchronized {
    factory.createSVGDocument(
      null,
      new ByteArrayInputStream(svgXml.getBytes("UTF-8"))
    )
  }

  import java.io.StringWriter
  import javax.xml.transform.TransformerFactory
  import javax.xml.transform.dom.DOMSource
  import javax.xml.transform.stream.StreamResult

  def toString(document: Document): IO[String] =
    IO {
      val tf = TransformerFactory.newInstance
      val trans = tf.newTransformer
      val sw = new StringWriter
      trans.transform(new DOMSource(document), new StreamResult(sw))
      sw.toString
    }
}

class SVGRendererJvm(options: SVGRendererOptions)(implicit
    appender: Element => generic.Frag[vdom.Builder[Element, Node], Node],
    matrixLike: SVGMatrixLike[AffineTransform],
    xmlDom: XmlDom
) extends SVGRenderer(xmlDom, options) {
  override def parseSvg(svgXml: String): Element =
    XmlDom.parseSvg(svgXml).getRootElement

  override protected val getPageMatrix: AffineTransform = new AffineTransform()
  override protected def getScreenCTM: AffineTransform = new AffineTransform()
  override protected def applyTransformation(transformation: AffineTransform): Unit = {
    getPageMatrix.setTransform(transformation)
    graphSVG.viewPort.setAttribute(
      "transform",
      s"matrix(${transformation.getScaleX} 0 0 ${transformation.getScaleY} ${transformation.getTranslateX} ${transformation.getTranslateY})"
    )
    Try(
      setDimensions(
        graphSVG.root.getAttribute("width").toInt,
        graphSVG.root.getAttribute("height").toInt
      )
    )
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

  override protected def renderSelectRect(
      elementId: String,
      elementType: String,
      x: Double,
      y: Double,
      width: Double,
      height: Double
  ): Option[Element] = None
  override protected def renderPanZoomRect: Option[Element] = None

  override def toXmlString: IO[String] = XmlDom.toString(xmlDom.svgDocument)

  override def setDimensions(width: Double, height: Double): Unit = {
    val padding = options.padding.getOrElse(0.0)
    val newWidth = (matrixLike.scalex(getPageMatrix) * width)
    val newHeight = (matrixLike.scaley(getPageMatrix) * height)

    graphSVG.root.setAttribute(
      "viewBox",
      s"${-padding / 2.0} ${-padding / 2.0} ${newWidth + padding} ${newHeight + padding}"
    )
    graphSVG.root.setAttribute("width", newWidth.toString)
    graphSVG.root.setAttribute("height", newHeight.toString)
  }

  override lazy val graphSVG: GraphSVG[Element] = renderRootSvg()
}

object SVGRendererJvm {
  private implicit val appender: Element => generic.Frag[vdom.Builder[Element, Node], Node] =
    elem =>
      new generic.Frag[vdom.Builder[Element, Node], Node] {
        override def render: Node = elem
        override def applyTo(t: Builder[Element, Node]): Unit =
          t.appendChild(elem)
      }

  private implicit val matrixLike: SVGMatrixLike[AffineTransform] =
    new SVGMatrixLike[AffineTransform] {
      override def identity: AffineTransform = new AffineTransform()
      override def inverse(matrix: AffineTransform): AffineTransform = matrix.createInverse()

      override def tx(matrix: AffineTransform): Double = matrix.getTranslateX
      override def tx_=(matrix: AffineTransform)(value: Double): Unit =
        matrix.setTransform(
          matrix.getScaleX,
          matrix.getShearY,
          matrix.getShearX,
          matrix.getScaleY,
          value,
          matrix.getTranslateY
        )

      override def ty(matrix: AffineTransform): Double = matrix.getTranslateY
      override def ty_=(matrix: AffineTransform)(value: Double): Unit =
        matrix.setTransform(
          matrix.getScaleX,
          matrix.getShearY,
          matrix.getShearX,
          matrix.getScaleY,
          matrix.getTranslateX,
          value
        )

      override def scalex(matrix: AffineTransform): Double = matrix.getScaleX
      override def scalex_=(matrix: AffineTransform)(value: Double): Unit =
        matrix.setTransform(
          value,
          matrix.getShearY,
          matrix.getShearX,
          matrix.getScaleY,
          matrix.getTranslateX,
          matrix.getTranslateY
        )

      override def scaley(matrix: AffineTransform): Double = matrix.getScaleY
      override def scaley_=(matrix: AffineTransform)(value: Double): Unit =
        matrix.setTransform(
          matrix.getScaleX,
          matrix.getShearY,
          matrix.getShearX,
          value,
          matrix.getTranslateX,
          matrix.getTranslateY
        )

      override def translate(matrix: AffineTransform)(dx: Double, dy: Double): AffineTransform = {
        val transform = new AffineTransform()
        transform.setToTranslation(dx, dy)
        matrix.concatenate(transform)
        matrix
      }

      override def scale(matrix: AffineTransform)(factor: Double): AffineTransform = {
        val transform = new AffineTransform()
        transform.setToScale(factor, factor)
        matrix.concatenate(transform)
        matrix
      }

      override def transformPoint(
          matrix: AffineTransform
      )(x: Double, y: Double): PagePoint = {
        val transformed = matrix.transform(new Point2D.Double(x, y), new Point2D.Double())
        PagePoint(transformed.getX, transformed.getY)
      }
    }

  private implicit def xmlDomInstance: XmlDom = new XmlDom(
    XmlDom.newSvgDocument
  )

  def apply(
      options: SVGRendererOptions = SVGRendererOptions()
  ): SVGRendererJvm = new SVGRendererJvm(options)
}
