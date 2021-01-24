package com.flowtick.graphs.editor.view

import cats.effect.IO
import com.flowtick.graphs.editor.util.DrawUtil
import com.flowtick.graphs.editor._
import com.flowtick.graphs.layout.{GraphLayoutLike, PointSpec}
import com.flowtick.graphs.style._
import com.flowtick.graphs.{Edge, Graph, Labeled, Node => GraphNode}
import scalatags.generic
import scalatags.generic.Bundle

import scala.util.Try

trait GraphElement[+T] {
  def id: ElementRef
  def group: T
  def selectElem: Option[T]
  def label: T
}

final class GraphSVG[+T](val root: T,
                         val panZoomRect: Option[T],
                         val viewPort: T,
                         val nodes: T,
                         val edges: T,
                         val select: T,
                         val label: T)

final case class SVGNodeElement[+T](id: ElementRef,
                                   shapeElement: T,
                                   label: T,
                                   selectElem: Option[T],
                                   group: T) extends GraphElement[T]

final case class SVGEdgeElement[+T](id: ElementRef,
                                   group: T,
                                   label: T,
                                   selectElem: Option[T]) extends GraphElement[T]

trait SVGRootLike

trait SVGMatrixLike[M] {
  def identity: M
  def inverse(matrix: M): M

  def tx(matrix: M): Double
  def tx_= (matrix: M)(value: Double): Unit

  def ty(matrix: M): Double
  def ty_= (matrix: M)(value: Double): Unit

  def scalex(matrix: M): Double
  def scalex_= (matrix: M)(value: Double): Unit

  def scaley(matrix: M): Double
  def scaley_= (matrix: M)(value: Double): Unit

  def translate(matrix: M)(dx: Double, dy: Double): M
  def scale(matrix: M)(factor: Double): M

  def transformPoint(matrix: M)(x: Double, y: Double): PagePoint
}

/**
 * a renderer for svg like models
 *
 * NOTE: this is built around the idea that the document model is mutable (so we can append nodes directly)
 * for efficiency. This is in line with the builder in scalatags for JsDom and vdom (see appendChild).
 *
 * @param bundle
 * @param appender
 * @param matrixLike
 * @tparam Builder
 * @tparam T
 * @tparam FragT
 * @tparam M
 */
abstract class SVGRenderer[Builder, T <: FragT, FragT, M](val bundle: Bundle[Builder, T, FragT])(implicit appender: T => generic.Frag[Builder, FragT], matrixLike: SVGMatrixLike[M]) {
  import bundle.all._
  import bundle.{svgAttrs, svgTags => svg}

  def x(elem: T): Double
  def y(elem: T): Double

  def setPosition(elem: T)(x: Double, y: Double): Unit

  def parseSvg(svgXml: String): T

  protected def getPageMatrix: M
  protected def getScreenCTM: M
  protected def applyTransformation(transformation: M): Unit

  def selectElement(value: GraphElement[T]): IO[Unit]
  def unselectElement(value: GraphElement[T]): IO[Unit]
  def deleteElement(element: GraphElement[T]): IO[Unit]

  def selectable(elem: T): Option[ElementRef]
  def draggable(elem: T): Option[ElementRef]

  def appendChild(elem: T)(child: T): Unit

  val graphSVG: GraphSVG[T] = renderRootSvg

  def resetMatrix: IO[Unit] = IO {
    applyTransformation(matrixLike.identity)
  }

  def setViewPortOffset(tx: Double, ty: Double): SVGRenderer[Builder, T, FragT, M] = {
    val matrix = getPageMatrix
    matrixLike.tx_=(matrix)(tx)
    matrixLike.ty_=(matrix)(ty)
    applyTransformation(matrix)
    this
  }

  def translateAndScaleView(tx: Double, ty: Double, zoom: Double = 1.0): SVGRenderer[Builder, T, FragT, M] = {
    applyTransformation(matrixLike.scale(matrixLike.translate(getPageMatrix)(tx, ty))(zoom))
    this
  }

  def getPageOffset: PagePoint = PagePoint(matrixLike.tx(getPageMatrix), matrixLike.ty(getPageMatrix))

  def screenCoordinates(x: Double, y: Double): PagePoint =
    matrixLike.transformPoint(matrixLike.inverse(getScreenCTM))(x, y)

  def pageCoordinates(x: Double, y: Double): PagePoint =
    matrixLike.transformPoint(matrixLike.inverse(getPageMatrix))(x, y)

  def renderGraph[E, N](graph: Graph[E, N],
                        layout: GraphLayoutLike,
                        styleSheet: StyleSheetLike)(implicit
                                                    nodeLabel: Labeled[N, String],
                                                    nodeStyleRef: StyleRef[GraphNode[N]],
                                                    edgeLabel: Labeled[E, String],
                                                    edgeStyleRef: StyleRef[Edge[E]]): IO[GraphSVG[T]] = {
    import cats.implicits._

    for {
      _ <- graph.nodes.map(renderNode(_, layout, styleSheet)).toList.sequence
      _ <- graph.edges.map(renderEdge(_, graph, layout, styleSheet)).toList.sequence
    } yield graphSVG
  }

  def renderNode[E, N](node: GraphNode[N], layout: GraphLayoutLike, styleSheet: StyleSheetLike)(implicit nodeLabel: Labeled[N, String], nodeStyleRef: StyleRef[GraphNode[N]]): IO[SVGNodeElement[T]] = for {
    nodeElement <- IO {
      val geometry = layout.nodeGeometry(node.id)
      val shape = styleSheet.requireNodeStyle(nodeStyleRef.id(node), nodeStyleRef.classList(node))

      val x = geometry.map(_.x).getOrElse(0.0)
      val y = geometry.map(_.y).getOrElse(0.0)

      val width = geometry.map(_.width).getOrElse(80.0)
      val height = geometry.map(_.height).getOrElse(50.0)

      val shapeElement: T = shape.svgContent.flatMap(content => styleSheet.images.get(content.refId)) match {
        case Some(image) if image.imageType == ImageType.svg => parseSvg(image.data)

        case _ => shapeTag(shape, styleSheet, width, height)(
          svgAttrs.style := nodeStyle(shape).mkString(";")
        ).render
      }
      val labelStyle = shape.labelStyle.getOrElse(NodeLabel())
      val elementType = "node"

      val label = renderLabel(node.id, elementType, labelStyle, nodeLabel(node.value), width, height)
      val selectRect = renderSelectRect(node.id, elementType, x, y, width, height)

      SVGNodeElement(
        ElementRef(node.id, NodeType),
        shapeElement,
        label,
        selectRect,
        svg.g(
          svgAttrs.transform := s"translate($x $y)",
          data("type") := "node",
          data("id") := node.id,
          shapeElement,
          label
        ).render
      )
    }
    _ <- IO {
      appendChild(graphSVG.nodes)(nodeElement.group)
      nodeElement.selectElem.foreach(selectElem => appendChild(graphSVG.select)(selectElem))
    }
  } yield nodeElement

  def renderLabel(elementId: String,
                  elementType: String,
                  labelStyle: LabelStyle,
                  labelValue: String,
                  width: Double,
                  height: Double,
                  offset: Option[PointSpec] = None): T = {
    val lines = labelValue.split("\n")
    val textCenterX = width / 2
    val textCenterY = (height / (1 + lines.size)) - Try(labelStyle.fontSize.getOrElse(defaultFontSize).toDouble * 0.6).getOrElse(10.0)
    val isFree = labelStyle.model.contains(Free)

    // TODO: instead of marking every tspan selectable, we could group the label

    val spans = lines.toList.map(line => svg.tspan(
        cls := "selectable",
        svgAttrs.x := 0,
        svgAttrs.textAnchor := (if (isFree) "start" else "middle"),
        svgAttrs.dominantBaseline := "alphabetic",
        svgAttrs.dy := "1.0em",
        data("id") := elementId,
        data("type") := elementType,
        line))

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
      svgAttrs.fontFamily := (if (labelStyle.fontFamily.contains("Dialog")) "Helvetica" else labelStyle.fontFamily.getOrElse(defaultFontFamily)), // Dialog does not exist in the most browser
      svgAttrs.fontSize := labelStyle.fontSize.getOrElse(defaultFontSize),
      svgAttrs.fill := fill,
      svgAttrs.stroke := labelStyle.border.map(_.color).getOrElse("#FFFFFF"),
      svgAttrs.strokeWidth := labelStyle.border.flatMap(_.width).getOrElse(0.0)
    )(spans: _*)

    text(labelStyle.textColor.getOrElse("#000000"))
  }.render

  protected def renderSelectRect(elementId: String,
                       elementType: String,
                       x: Double,
                       y: Double,
                       width: Double,
                       height: Double): Option[T] = {
    val offset = 6
    Some(svg.rect(
      cls := "selectable",
      svgAttrs.width :=  width + offset,
      svgAttrs.height := height + offset,
      svgAttrs.x := - offset / 2,
      svgAttrs.y := - offset / 2,
      svgAttrs.fill := "white",
      svgAttrs.fillOpacity := 0,
      svgAttrs.style := "cursor: pointer",
      data("id") := elementId,
      data("type") := elementType,
      svgAttrs.transform := s"translate($x $y)"
    ).render)
  }

  def renderEdge[E, N](edge: Edge[E],
                       graph: Graph[E, N],
                       layout: GraphLayoutLike,
                       styleSheet: StyleSheetLike)(implicit edgeLabel: Labeled[E, String], edgeStyleRef: StyleRef[Edge[E]]): IO[Option[SVGEdgeElement[T]]] = for {
    edgeElementOpt <- IO {
      for {
        points <- DrawUtil.getLinePoints(edge, graph, layout).map(_.toList)
        start <- points.headOption
        end <- points.reverse.headOption

        pointsString = points.map(p => s"${p.x} ${p.y}").mkString(", ")
        style = styleSheet.getEdgeStyle(edgeStyleRef.id(edge), edgeStyleRef.classList(edge)).getOrElse(EdgeShape())

        line = {
          svg.polyline(
            svgAttrs.points := pointsString,
            svgAttrs.style := s"fill:none",
            svgAttrs.stroke := style.edgeStyle.map(_.color).getOrElse("#000000"),
            svgAttrs.strokeWidth := style.edgeStyle.flatMap(_.width).getOrElse(1.0),
            svgAttrs.markerStart := style.arrows.flatMap(_.source).map(source => s"url(#arrow_${source})").getOrElse(""),
            svgAttrs.markerEnd :=  style.arrows.flatMap(_.target).map(target => s"url(#arrow_${target})").getOrElse("")
          ).render
        }

        selectLine = {
          val polyline = svg.polyline(
            cls := "selectable",
            svgAttrs.points := pointsString,
            svgAttrs.style := s"stroke:#ffffff;stroke-width:15px;fill:none;cursor:pointer",
            svgAttrs.strokeOpacity := 0.0,
            data("id") := edge.id,
            data("type") := "edge"
          ).render

          polyline
        }

        label =  {
          val width = Math.abs(start.x - end.x)
          val height = Math.abs(end.y - start.y)
          val edgeLabelStyle = style.labelStyle.getOrElse(EdgeLabel())
          val bendPoints = layout.edgePath(edge.id).map(_.points).filter(_.nonEmpty)
          val stylePosition = edgeLabelStyle.position

          val bendPointsOrStyleOrMiddle = bendPoints
            .orElse(stylePosition.map(stylePos => List(PointSpec(stylePos.x, stylePos.y))))
            .getOrElse(List(PointSpec((end.x - start.x) / 2.0, (end.y - start.y) / 2.0)))

          val labelPos = {
            val sumPoint = bendPointsOrStyleOrMiddle.foldLeft(PointSpec(0.0, 0.0))((acc, next) => acc.copy(x = acc.x + next.x, y = acc.y + next.y))
            sumPoint.copy(x = sumPoint.x / bendPointsOrStyleOrMiddle.size, y = sumPoint.y / bendPointsOrStyleOrMiddle.size)
          }

          val finalModel = if (bendPoints.isDefined) Some(Free) else edgeLabelStyle.model // layout wins
          val finalStyle = edgeLabelStyle.copy(position = Some(StylePos(labelPos.x, labelPos.y)), model = finalModel)
          val labelString = edgeLabel(edge.value)

          renderLabel(edge.id, "edge", finalStyle, labelString, width, height, if(bendPoints.isDefined) None else Some(start))
        }

        selectGroup = svg.g(id := edge.id, line, selectLine).render
      } yield SVGEdgeElement(ElementRef(edge.id, EdgeType), selectGroup, label, Some(selectGroup))
    }
    _ <- IO {
      edgeElementOpt.foreach { edgeElement =>
        appendChild(graphSVG.edges)(edgeElement.group)
        appendChild(graphSVG.label)(edgeElement.label)
      }
    }
  } yield edgeElementOpt

  protected def renderPanZoomRect: Option[T] = Some(svg.rect(
    id := "pan-zoom-hit",
    svgAttrs.width := "100%",
    svgAttrs.height := "100%",
    svgAttrs.fill := "none",
    svgAttrs.style := "pointer-events:all"
  ).render)


  protected def renderOriginMarker: Option[T] = {
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
      ),
    )
    Some(originMarker.render)
  }
  protected def renderRootSvg: GraphSVG[T] = {
    import bundle.all._
    import bundle.{svgAttrs, svgTags => svg}

    val panZoomRect = renderPanZoomRect

    val edges = svg.g(id := "edges").render
    val nodes = svg.g(id := "nodes").render
    val select = svg.g(id := "select").render
    val label = svg.g(id := "label").render

    val viewPort = svg.g(
      id := "viewport",
      svgAttrs.transform := "matrix(1 0 0 1 0 0)",
      renderOriginMarker,
      edges,
      nodes,
      select,
      label
    ).render

    val svgElem = svg.svg(
      svg.defs(
        svg.marker(
          svgAttrs.id := "arrow_standard",
          svgAttrs.viewBox := "0 0 10 10",
          svgAttrs.refX := "10",
          svgAttrs.refY := "5",
          svgAttrs.markerWidth := "10",
          svgAttrs.markerHeight := "10",
          svgAttrs.orient := "auto-start-reverse",
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
      svgAttrs.preserveAspectRatio := "meet",
      panZoomRect,
      viewPort
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

  val defaultTextColor = "#000000"
  val defaultFontSize = "12"
  val defaultFontFamily = "Helvetica"

  def shapeTag(shape: NodeShape,
                                                                                        styleSheet: com.flowtick.graphs.style.StyleSheetLike,
                                                                                        widthValue: Double,
                                                                                        heightValue: Double): generic.TypedTag[Builder, T, FragT] = {
    import bundle.all._
    import bundle.{svgAttrs, svgTags => svg}

    lazy val fallback = svg.rect(
      svgAttrs.width :=  widthValue,
      svgAttrs.height := heightValue,
      svgAttrs.x := 0,
      svgAttrs.y := 0,
    )

    lazy val vectorShape = shape.shapeType match {
      case Some(ShapeType.Ellipse) => svg.ellipse(
        svgAttrs.cx := widthValue / 2.0,
        svgAttrs.cy := heightValue / 2.0,
        svgAttrs.rx := widthValue / 2.0,
        svgAttrs.ry := heightValue / 2.0
      )

      case Some(ShapeType.RoundRectangle) => svg.rect(
        svgAttrs.rx := 5,
        svgAttrs.ry := 5,
        svgAttrs.width :=  widthValue,
        svgAttrs.height := heightValue,
        svgAttrs.x := 0,
        svgAttrs.y := 0,
      )

      case _ => fallback
    }

    shape.image.flatMap(img => styleSheet.images.get(img)) match {
      case Some(imageResource) if imageResource.imageType == ImageType.svg =>
        svg.image(
          svgAttrs.width :=  widthValue,
          svgAttrs.height := heightValue,
          svgAttrs.xLinkHref := s"data:application/svg+xml,${imageResource.data.replaceAll("\n", "")}"
        )
      case Some(imageUrl) if imageUrl.imageType == ImageType.dataUrl || imageUrl.imageType == ImageType.url  => svg.image(
        svgAttrs.width :=  widthValue,
        svgAttrs.height := heightValue,
        svgAttrs.xLinkHref := imageUrl.data
      )
      case _ => vectorShape
    }
  }

  def nodeStyle(shape: NodeShape): Seq[String] = {
    val borderProps: Option[List[String]] = for {
      bs <- shape.borderStyle.orElse(Some(BorderStyle("#000000", styleType = Some("line"), width = Some(1.0))))
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