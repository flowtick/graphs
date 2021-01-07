package com.flowtick.graphs.editor

import com.flowtick.graphs.{Node => GraphNode}
import com.flowtick.graphs.Edge
import com.flowtick.graphs.editor.SVGGraphRenderer.SVGGraphElement
import com.flowtick.graphs.editor.util.DrawUtil
import com.flowtick.graphs.style._
import org.scalajs.dom.raw._
import org.scalajs.dom.svg.{G, RectElement, Text}
import scalatags.JsDom
import scalatags.JsDom.all._
import scalatags.JsDom.{svgAttrs, svgTags => svg}

import scala.util.Try

final case class SVGNodeElement(id: ElementRef,
                          shapeElement: SVGGraphElement,
                          label: Text,
                          selectElem: SVGElement,
                          group: G) extends GraphElement[SVGElement]

final case class SVGEdgeElement(id: ElementRef,
                          group: G,
                          label: Text,
                          selectElem: SVGElement) extends GraphElement[SVGElement]

object SVGGraphRenderer {
  val defaultTextColor = "#000000"
  val defaultFontSize = "12"
  val defaultFontFamily = "Helvetica"

  type SVGGraphElement = SVGElement

  def setSelection(element: GraphElement[SVGElement]): Unit = {
    element.selectElem.setAttribute("stroke", "#555555")
    element.selectElem.setAttribute("stroke-dasharray", "3 5")
    element.selectElem.classList.add("draggable")
  }

  def unsetSelection(element: GraphElement[SVGElement]): Unit = {
    element.selectElem.setAttribute("stroke", null)
    element.selectElem.setAttribute("stroke-dasharray", null)
    element.selectElem.classList.remove("draggable")
  }

  def renderNode(node: GraphNode[EditorGraphNode], model: EditorModel): SVGNodeElement = {
    val geometry = model.layout.nodeGeometry(node.id)
    val shape = model.styleSheet.requireNodeStyle(Some(node.id), node.value.stencil.toList)

    val x = geometry.map(_.x).getOrElse(0.0)
    val y = geometry.map(_.y).getOrElse(0.0)

    val width = geometry.map(_.width).getOrElse(80.0)
    val height = geometry.map(_.height).getOrElse(50.0)

    val shapeElement = shape.svgContent.flatMap(content => model.styleSheet.images.get(content.refId)) match {
      case Some(image) if image.imageType == "svg" =>
        new DOMParser()
          .parseFromString(image.data, "application/xml")
          .firstChild
          .asInstanceOf[SVGElement]

      case _ => shapeTag(shape, model.styleSheet, width, height)(
        svgAttrs.style := nodeStyle(shape).mkString(";"),
      ).render
    }
    val labelStyle = shape.labelStyle.getOrElse(NodeLabel())

    val label: Text = renderLabel(node.id, "node", labelStyle, node.value.label.getOrElse(""), width, height)

    val selectRect = SVGGraphRenderer.renderSelectRect(x, y, width, height)
    selectRect.setAttribute("data-id", node.id)
    selectRect.setAttribute("data-type", "node")

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

  def shapeTag(shape: NodeShape,
               styleSheet: com.flowtick.graphs.style.StyleSheetLike,
               width: Double,
               height: Double): JsDom.TypedTag[SVGGraphElement] = {
    lazy val fallback = svg.rect(
      svgAttrs.width :=  width,
      svgAttrs.height := height,
      svgAttrs.x := 0,
      svgAttrs.y := 0,
    )

    lazy val vectorShape = shape.shapeType match {
      case Some(ShapeType.Ellipse) => svg.ellipse(
        svgAttrs.cx := width / 2.0,
        svgAttrs.cy := height / 2.0,
        svgAttrs.rx := width / 2.0,
        svgAttrs.ry := height / 2.0
      )

      case Some(ShapeType.RoundRectangle) => svg.rect(
        svgAttrs.rx := 5,
        svgAttrs.ry := 5,
        svgAttrs.width :=  width,
        svgAttrs.height := height,
        svgAttrs.x := 0,
        svgAttrs.y := 0,
      )

      case _ => fallback
    }

    shape.image.flatMap(img => styleSheet.images.get(img)) match {
      case Some(imageResource) if imageResource.imageType == "svg" =>
        svg.image(
          svgAttrs.width :=  width,
          svgAttrs.height := height,
          svgAttrs.xLinkHref := s"data:application/svg+xml,${imageResource.data.replaceAll("\n", "")}"
        )
      case Some(imageUrl) if imageUrl.imageType == "dataUrl" || imageUrl.imageType == "url" => svg.image(
        svgAttrs.width :=  width,
        svgAttrs.height := height,
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

  def renderSelectRect(x: Double,
                       y: Double,
                       width: Double,
                       height: Double): RectElement = {
    val offset = 6
    val select = svg.rect(
      cls := "selectable",
      svgAttrs.width :=  width + offset,
      svgAttrs.height := height + offset,
      svgAttrs.x := - offset / 2,
      svgAttrs.y := - offset / 2,
      svgAttrs.fill := "white",
      svgAttrs.fillOpacity := 0,
      svgAttrs.style := "cursor: pointer"
    ).render

    SVGUtil.setTransform(select, s"translate($x $y)")
    select
  }

  def renderLabel(elementId: String,
                  elementType: String,
                  labelStyle: LabelStyle,
                  labelValue: String,
                  width: Double,
                  height: Double): Text = {
    val lines = labelValue.split("\n")
    val textCenterX = width / 2
    val textCenterY = (height / (1 + lines.size)) - Try(labelStyle.fontSize.getOrElse(defaultFontSize).toDouble * 0.6).getOrElse(10.0)
    val isFree = labelStyle.model.contains(Free)

    // TODO: instead of marking every tspan selectable, we could group the label

    val spans = lines
      .map(line => svg.tspan(
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
      } yield (pos.x, pos.y)).getOrElse((0.0, 0.0))
    } else (textCenterX, textCenterY)

    svg.text(
      svgAttrs.transform := s"translate($finalX $finalY)",
      svgAttrs.textAnchor := "start",
      svgAttrs.fontFamily := (if (labelStyle.fontFamily.contains("Dialog")) "Helvetica" else labelStyle.fontFamily.getOrElse(defaultFontFamily)), // Dialog does not exist in the most browser
      svgAttrs.fontSize := labelStyle.fontSize.getOrElse(defaultFontSize),
      svgAttrs.fill := labelStyle.textColor.getOrElse(defaultTextColor),
    )(spans: _*)
  }.render

  def renderEdge(edge: Edge[EditorGraphEdge],
                 model: EditorModel): Option[SVGEdgeElement] = {
    for {
      points <- DrawUtil.getLinePoints(edge, model.graph, model.layout).map(_.toList)
      start <- points.headOption
      end <- points.reverse.headOption

      pointsString = points.map(p => s"${p.x} ${p.y}").mkString(", ")
      style <- model.styleSheet.getEdgeStyle(Some(edge.id), edge.value.connector.toList)

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
          svgAttrs.strokeOpacity := 0.0
        ).render

        polyline.setAttribute("data-id", edge.id)
        polyline.setAttribute("data-type", "edge")

        polyline
      }

      label =  {
        val bbox = line.getBBox()
        for {
          edgeLabel <- style.labelStyle
          pos <- edgeLabel.position.orElse(Some(PointSpec((end.x - start.x) / 2, (end.y - start.y) / 2)))
          withStartPoint = edgeLabel.copy(position = Some(PointSpec(pos.x + start.x, pos.y + start.y)))
        } yield renderLabel(edge.id, "edge", withStartPoint, edge.value.label.getOrElse(""), bbox.width, bbox.height)
      }.getOrElse(svg.text("").render)

      group = svg.g(id := edge.id, line, selectLine).render

    } yield SVGEdgeElement(ElementRef(edge.id, EdgeType), group, label, group)
  }

}
