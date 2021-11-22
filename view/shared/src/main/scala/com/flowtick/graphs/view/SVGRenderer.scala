package com.flowtick.graphs.view

import cats.effect.IO
import cats.implicits.toTraverseOps
import com.flowtick.graphs.layout.PointSpec
import com.flowtick.graphs.style._
import com.flowtick.graphs.view.util.DrawUtil
import com.flowtick.graphs.{Edge, Graph, Labeled, Node => GraphNode}
import scalatags.generic
import scalatags.generic.Bundle

import scala.util.Try

final class GraphSVG[+T](
    val root: T,
    val panZoomRect: Option[T],
    val viewPort: T,
    val nodes: T,
    val edges: T,
    val select: T,
    val label: T
)

final case class SVGNodeElement[+T](
    id: ElementRef,
    shapeElement: T,
    label: T,
    selectElem: Option[T],
    group: T
) extends GraphElement[T]

final case class SVGEdgeElement[+T](
    id: ElementRef,
    group: T,
    label: T,
    selectElem: Option[T]
) extends GraphElement[T]

trait SVGRootLike

trait SVGMatrixLike[M] {
  def identity: M

  def inverse(matrix: M): M

  def tx(matrix: M): Double

  def tx_=(matrix: M)(value: Double): Unit

  def ty(matrix: M): Double

  def ty_=(matrix: M)(value: Double): Unit

  def scalex(matrix: M): Double

  def scalex_=(matrix: M)(value: Double): Unit

  def scaley(matrix: M): Double

  def scaley_=(matrix: M)(value: Double): Unit

  def translate(matrix: M)(dx: Double, dy: Double): M

  def scale(matrix: M)(factor: Double): M

  def transformPoint(matrix: M)(x: Double, y: Double): PagePoint
}

final case class SVGRendererOptions(
    showOrigin: Boolean = false,
    padding: Option[Double] = None,
    defaultTextColor: String = "#000000",
    defaultFontSize: String = "12",
    defaultFontFamily: String = "Helvetica"
)

/** a renderer for svg like models
  *
  * NOTE: this is built around the idea that the document model is mutable (so we can append nodes
  * directly) for efficiency. This is in line with the builder in scalatags for JsDom and vdom (see
  * appendChild).
  *
  * @param bundle
  * @param appender
  * @param matrixLike
  * @tparam Builder
  * @tparam Output
  * @tparam FragT
  * @tparam MatrixType
  */
abstract class SVGRenderer[Builder, Output <: FragT, FragT, MatrixType](
    protected val bundle: Bundle[Builder, Output, FragT],
    protected val options: SVGRendererOptions
)(implicit
    appender: Output => generic.Frag[Builder, FragT],
    matrixLike: SVGMatrixLike[MatrixType]
) {
  import bundle.all._
  import bundle.{svgAttrs, svgTags => svg}

  def x(elem: Output): Double

  def y(elem: Output): Double

  def setPosition(elem: Output)(x: Double, y: Double): Unit

  def setDimensions(width: Double, height: Double): Unit

  def parseSvg(svgXml: String): Output

  protected def getPageMatrix: MatrixType

  protected def getScreenCTM: MatrixType

  protected def applyTransformation(transformation: MatrixType): Unit

  def selectElement(value: GraphElement[Output]): IO[Unit]

  def unselectElement(value: GraphElement[Output]): IO[Unit]

  def deleteElement(element: GraphElement[Output]): IO[Unit]

  def selectable(elem: Output): Option[ElementRef]

  def draggable(elem: Output): Option[ElementRef]

  def appendChild(elem: Output)(child: Output): Unit

  def graphSVG: GraphSVG[Output]

  def toXmlString: IO[String]

  def resetMatrix: IO[Unit] = IO {
    applyTransformation(matrixLike.identity)
  }

  def setViewPortOffset(
      tx: Double,
      ty: Double
  ): SVGRenderer[Builder, Output, FragT, MatrixType] = {
    val matrix = getPageMatrix
    matrixLike.tx_=(matrix)(tx)
    matrixLike.ty_=(matrix)(ty)
    applyTransformation(matrix)
    this
  }

  def translateAndScaleView(
      tx: Double,
      ty: Double,
      zoom: Double = 1.0
  ): SVGRenderer[Builder, Output, FragT, MatrixType] = {
    applyTransformation(
      matrixLike.scale(matrixLike.translate(getPageMatrix)(tx, ty))(zoom)
    )
    this
  }

  def getPageOffset: PagePoint =
    PagePoint(matrixLike.tx(getPageMatrix), matrixLike.ty(getPageMatrix))

  def screenCoordinates(x: Double, y: Double): PagePoint =
    matrixLike.transformPoint(matrixLike.inverse(getScreenCTM))(x, y)

  def pageCoordinates(x: Double, y: Double): PagePoint =
    matrixLike.transformPoint(matrixLike.inverse(getPageMatrix))(x, y)

  def renderGraph[E, N](
      model: ViewContextLike[E, N]
  ): IO[SVGRenderer[Builder, Output, FragT, MatrixType]] = {
    for {
      _ <- IO {
        val width = model.layout.width.getOrElse(400.0)
        val height = model.layout.height.getOrElse(400.0)
        setDimensions(width, height)
      }
      _ <- model.graph.nodes.map(renderNode(_, model)).toList.sequence
      _ <- model.graph.edges
        .map(renderEdge(_, model))
        .toList
        .sequence
    } yield this
  }

  def renderNode[E, N](
      node: GraphNode[N],
      model: ViewContextLike[E, N]
  ): IO[SVGNodeElement[Output]] = for {
    nodeElement <- IO {
      val geometry = model.layout.nodeGeometry(node.id)
      val shape = model.styleSheet.requireNodeStyle(
        model.nodeStyleRef.id(node),
        model.nodeStyleRef.classList(node)
      )

      val x = geometry.map(_.x).getOrElse(0.0)
      val y = geometry.map(_.y).getOrElse(0.0)

      val width = geometry.map(_.width).getOrElse(80.0)
      val height = geometry.map(_.height).getOrElse(50.0)

      val shapeElement: Output =
        shape.svgContent.flatMap(content => model.styleSheet.images.get(content.refId)) match {
          case Some(image) if image.imageType == ImageType.svg =>
            parseSvg(image.data)

          case _ =>
            shapeTag(shape, model.styleSheet, width, height)(
              svgAttrs.style := nodeStyle(shape).mkString(";")
            ).render
        }
      val labelStyle = shape.labelStyle.getOrElse(NodeLabel())
      val elementType = "node"

      val label = renderLabel(
        node.id,
        elementType,
        labelStyle,
        model.nodeLabel(node.value),
        width,
        height
      )
      val selectRect =
        renderSelectRect(node.id, elementType, x, y, width, height)

      SVGNodeElement(
        ElementRef(node.id, NodeElementType),
        shapeElement,
        label,
        selectRect,
        svg
          .g(
            svgAttrs.transform := s"translate($x $y)",
            data("type") := "node",
            data("id") := node.id,
            shapeElement,
            label
          )
          .render
      )
    }
    _ <- IO {
      appendChild(graphSVG.nodes)(nodeElement.group)
      nodeElement.selectElem.foreach(selectElem => appendChild(graphSVG.select)(selectElem))
    }
  } yield nodeElement

  def renderLabel(
      elementId: String,
      elementType: String,
      labelStyle: LabelStyle,
      labelValue: String,
      width: Double,
      height: Double,
      offset: Option[PointSpec] = None
  ): Output = {
    val lines = labelValue.split("\n")
    val textCenterX = width / 2
    val textCenterY = (height / (1 + lines.size)) - Try(
      labelStyle.fontSize.getOrElse(options.defaultFontSize).toDouble * 0.6
    ).getOrElse(10.0)
    val isFree = labelStyle.model.contains(Free)

    // TODO: instead of marking every tspan selectable, we could group the label

    val spans = lines.toList.map(line =>
      svg.tspan(
        cls := "selectable",
        svgAttrs.x := 0,
        svgAttrs.textAnchor := (if (isFree) "start" else "middle"),
        svgAttrs.dominantBaseline := "alphabetic",
        svgAttrs.dy := "1.0em",
        data("id") := elementId,
        data("type") := elementType,
        line
      )
    )

    val (finalX: Double, finalY: Double) = if (isFree) {
      (for {
        pos <- labelStyle.position
        offsetX = offset.map(_.x).getOrElse(0.0)
        offsetY = offset.map(_.y).getOrElse(0.0)
      } yield (pos.x + offsetX, pos.y + offsetY)).getOrElse((0.0, 0.0))
    } else (textCenterX, textCenterY)

    def text(fill: String) = svg.text(
      svgAttrs.transform := s"translate($finalX $finalY)",
      svgAttrs.textAnchor := "start",
      svgAttrs.fontFamily := (if (labelStyle.fontFamily.contains("Dialog"))
                                "Helvetica"
                              else
                                labelStyle.fontFamily.getOrElse(
                                  options.defaultFontFamily
                                )), // Dialog does not exist in the most browser
      svgAttrs.fontSize := labelStyle.fontSize.getOrElse(options.defaultFontSize),
      svgAttrs.fill := fill,
      svgAttrs.stroke := labelStyle.border.map(_.color).getOrElse("#FFFFFF"),
      svgAttrs.strokeWidth := labelStyle.border.flatMap(_.width).getOrElse(0.0)
    )(spans: _*)

    text(labelStyle.textColor.getOrElse(options.defaultTextColor))
  }.render

  protected def renderSelectRect(
      elementId: String,
      elementType: String,
      x: Double,
      y: Double,
      width: Double,
      height: Double
  ): Option[Output] = {
    val offset = 6
    Some(
      svg
        .rect(
          cls := "selectable",
          svgAttrs.width := width + offset,
          svgAttrs.height := height + offset,
          svgAttrs.x := -offset / 2,
          svgAttrs.y := -offset / 2,
          svgAttrs.fill := "white",
          svgAttrs.fillOpacity := 0,
          svgAttrs.style := "cursor: pointer",
          data("id") := elementId,
          data("type") := elementType,
          svgAttrs.transform := s"translate($x $y)"
        )
        .render
    )
  }

  def renderEdge[E, N](
      edge: Edge[E],
      model: ViewContextLike[E, N]
  ): IO[Option[SVGEdgeElement[Output]]] = for {
    edgeElementOpt <- IO {
      for {
        points <- DrawUtil.getLinePoints(edge, model.graph, model.layout).map(_.toList)
        start <- points.headOption
        end <- points.reverse.headOption

        pointsString = points.map(p => s"${p.x} ${p.y}").mkString(", ")
        style = model.styleSheet
          .getEdgeStyle(model.edgeStyleRef.id(edge), model.edgeStyleRef.classList(edge))
          .getOrElse(EdgeShape())

        line = {
          svg
            .polyline(
              svgAttrs.points := pointsString,
              svgAttrs.style := s"fill:none",
              svgAttrs.stroke := style.edgeStyle
                .map(_.color)
                .getOrElse("#000000"),
              svgAttrs.strokeWidth := style.edgeStyle
                .flatMap(_.width)
                .getOrElse(1.0),
              svgAttrs.markerStart := style.arrows
                .flatMap(_.source)
                .map(source => s"url(#arrow_${source})")
                .getOrElse("none"),
              svgAttrs.markerEnd := style.arrows
                .flatMap(_.target)
                .map(target => s"url(#arrow_${target})")
                .getOrElse("none")
            )
            .render
        }

        selectLine = {
          val polyline = svg
            .polyline(
              cls := "selectable",
              svgAttrs.points := pointsString,
              svgAttrs.style := s"stroke:#ffffff;stroke-width:15px;fill:none;cursor:pointer",
              svgAttrs.strokeOpacity := 0.0,
              data("id") := edge.id,
              data("type") := "edge"
            )
            .render

          polyline
        }

        label = {
          val width = Math.abs(start.x - end.x)
          val height = Math.abs(end.y - start.y)
          val edgeLabelStyle = style.labelStyle.getOrElse(EdgeLabel())
          val bendPoints =
            model.layout.edgePath(edge.id).map(_.points).filter(_.nonEmpty)
          val stylePosition = edgeLabelStyle.position

          val bendPointsOrStyleOrMiddle = bendPoints
            .orElse(
              stylePosition.map(stylePos => List(PointSpec(stylePos.x, stylePos.y)))
            )
            .getOrElse(
              List(PointSpec((end.x - start.x) / 2.0, (end.y - start.y) / 2.0))
            )

          val labelPos = {
            val sumPoint = bendPointsOrStyleOrMiddle.foldLeft(
              PointSpec(0.0, 0.0)
            )((acc, next) => acc.copy(x = acc.x + next.x, y = acc.y + next.y))
            sumPoint.copy(
              x = sumPoint.x / bendPointsOrStyleOrMiddle.size,
              y = sumPoint.y / bendPointsOrStyleOrMiddle.size
            )
          }

          val finalModel =
            if (bendPoints.isDefined) Some(Free)
            else edgeLabelStyle.model // layout wins
          val finalStyle = edgeLabelStyle.copy(
            position = Some(StylePos(labelPos.x, labelPos.y)),
            model = finalModel
          )
          val labelString = model.edgeLabel(edge.value)

          renderLabel(
            edge.id,
            "edge",
            finalStyle,
            labelString,
            width,
            height,
            if (bendPoints.isDefined) None else Some(start)
          )
        }

        selectGroup = svg.g(id := edge.id, line, selectLine).render
      } yield SVGEdgeElement(
        ElementRef(edge.id, EdgeElementType),
        selectGroup,
        label,
        Some(selectGroup)
      )
    }
    _ <- IO {
      edgeElementOpt.foreach { edgeElement =>
        appendChild(graphSVG.edges)(edgeElement.group)
        appendChild(graphSVG.label)(edgeElement.label)
      }
    }
  } yield edgeElementOpt

  protected def renderPanZoomRect: Option[Output] = Some(
    svg
      .rect(
        id := "pan-zoom-hit",
        svgAttrs.width := "100%",
        svgAttrs.height := "100%",
        svgAttrs.fill := "none",
        svgAttrs.style := "pointer-events:all"
      )
      .render
  )

  protected def renderOriginMarker: Option[Output] = if (options.showOrigin) {
    val originStroke = "#CCC"
    val originStrokeWidth = 2
    val originDim = 5
    val originMarker = svg.g(
      id := "origin",
      svg.line(
        svgAttrs.strokeWidth := originStrokeWidth,
        svgAttrs.x1 := -originDim,
        svgAttrs.x2 := originDim,
        svgAttrs.y1 := 0,
        svgAttrs.y2 := 0,
        svgAttrs.stroke := originStroke
      ),
      svg.line(
        svgAttrs.strokeWidth := originStrokeWidth,
        svgAttrs.y1 := -originDim,
        svgAttrs.y2 := originDim,
        svgAttrs.x1 := 0,
        svgAttrs.x2 := 0,
        svgAttrs.stroke := originStroke
      )
    )
    Some(originMarker.render)
  } else None

  protected def renderRootSvg(rootAttributes: Modifier*): GraphSVG[Output] = {
    import bundle.all._

    val panZoomRect = renderPanZoomRect

    val edges = svg.g(id := "edges").render
    val nodes = svg.g(id := "nodes").render
    val select = svg.g(id := "select").render
    val label = svg.g(id := "label").render

    val viewPort = svg
      .g(
        id := "viewport",
        svgAttrs.transform := "matrix(1 0 0 1 0 0)",
        renderOriginMarker,
        edges,
        nodes,
        select,
        label
      )
      .render

    val svgElem = svg.svg(
      svg.defs(
        svg.marker(
          svgAttrs.id := "arrow_standard",
          svgAttrs.viewBox := "0 0 10 10",
          svgAttrs.refX := "10",
          svgAttrs.refY := "5",
          svgAttrs.markerWidth := "10",
          svgAttrs.markerHeight := "10",
          svgAttrs.orient := "auto",
          svg.path(svgAttrs.d := "M 0 0 L 10 5 L 0 10 z")
        ),
        svg.marker(
          svgAttrs.id := "arrow_circle",
          svgAttrs.viewBox := "0 0 10 10",
          svgAttrs.refX := "5",
          svgAttrs.refY := "5",
          svgAttrs.markerWidth := "8",
          svgAttrs.markerHeight := "8",
          svg.circle(
            svgAttrs.cx := 5,
            svgAttrs.cy := 5,
            svgAttrs.r := 5,
            svgAttrs.fill := "black"
          )
        )
      ),
      panZoomRect,
      viewPort,
      rootAttributes
    )

    new GraphSVG(
      root = svgElem.render,
      panZoomRect = panZoomRect,
      viewPort = viewPort,
      nodes = nodes,
      edges = edges,
      select = select,
      label = label
    )
  }

  def shapeTag(
      shape: NodeShape,
      styleSheet: com.flowtick.graphs.style.StyleSheetLike,
      widthValue: Double,
      heightValue: Double
  ): generic.TypedTag[Builder, Output, FragT] = {
    import bundle.all._

    lazy val fallback = svg.rect(
      svgAttrs.width := widthValue,
      svgAttrs.height := heightValue,
      svgAttrs.x := 0,
      svgAttrs.y := 0
    )

    lazy val vectorShape = shape.shapeType match {
      case Some(ShapeType.Ellipse) =>
        svg.ellipse(
          svgAttrs.cx := widthValue / 2.0,
          svgAttrs.cy := heightValue / 2.0,
          svgAttrs.rx := widthValue / 2.0,
          svgAttrs.ry := heightValue / 2.0
        )

      case Some(ShapeType.RoundRectangle) =>
        svg.rect(
          svgAttrs.rx := 5,
          svgAttrs.ry := 5,
          svgAttrs.width := widthValue,
          svgAttrs.height := heightValue,
          svgAttrs.x := 0,
          svgAttrs.y := 0
        )

      case _ => fallback
    }

    shape.image.flatMap(img => styleSheet.images.get(img)) match {
      case Some(imageResource) if imageResource.imageType == ImageType.svg =>
        svg.image(
          svgAttrs.width := widthValue,
          svgAttrs.height := heightValue,
          svgAttrs.xLinkHref := s"data:application/svg+xml,${imageResource.data
            .replaceAll("\n", "")}"
        )
      case Some(imageUrl)
          if imageUrl.imageType == ImageType.dataUrl || imageUrl.imageType == ImageType.url =>
        svg.image(
          svgAttrs.width := widthValue,
          svgAttrs.height := heightValue,
          svgAttrs.xLinkHref := imageUrl.data
        )
      case _ => vectorShape
    }
  }

  def nodeStyle(shape: NodeShape): Seq[String] = {
    val borderProps: Option[List[String]] = for {
      bs <- shape.borderStyle.orElse(
        Some(
          BorderStyle("#000000", styleType = Some("line"), width = Some(1.0))
        )
      )
    } yield List(
      s"stroke:${bs.color}",
      s"stroke-width:${bs.width.getOrElse(1.0)}"
    )

    val fillProps: Option[List[String]] = for {
      f <- shape.fill.orElse(Some(Fill(None)))
      color <- f.color.orElse(Some("#FFFFFF"))
    } yield List(
      s"fill:$color",
      "fill-opacity:".concat(if (color.isEmpty) "0.0" else "1.0")
    )

    fillProps.getOrElse(List.empty) ++ borderProps.getOrElse(List.empty)
  }

}
