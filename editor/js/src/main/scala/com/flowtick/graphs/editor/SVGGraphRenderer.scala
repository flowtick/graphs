package com.flowtick.graphs.editor

import com.flowtick.graphs.{DrawUtil, Edge, EdgeType, ElementType, GraphElement, NodeType}
import com.flowtick.graphs.editor.SVGGraphRenderer.SVGGraphElement
import com.flowtick.graphs.graphml.{BorderStyle, EdgePath, EdgeShape, Fill, Free, GraphMLEdge, GraphMLGraph, GraphMLNode, GraphMLResource, LabelLike, NodeShape, PointSpec, ShapeType}
import io.circe.Json
import org.scalajs.dom.raw._
import org.scalajs.dom.svg.{G, RectElement, Text}
import scalatags.JsDom
import scalatags.JsDom.all._
import scalatags.JsDom.{svgAttrs, svgTags => svg}

import scala.util.Try


case class Troll(weapon: String)

case class SVGNodeElement(id: String,
                          shapeElement: SVGGraphElement,
                          label: Text,
                          selectElem: SVGElement,
                          group: G) extends GraphElement[SVGElement] {
  override def elementType: ElementType = NodeType
}

case class SVGEdgeElement(id: String,
                          group: G,
                          label: Text,
                          selectElem: SVGElement) extends GraphElement[SVGElement] {
  override def elementType: ElementType = EdgeType
}

object SVGGraphRenderer {
  val defaultTextColor = "#000000"
  val defaultFontSize = "12"
  val defaultFontFamily = "Helvetica"

  type SVGGraphElement = SVGElement

  def setSelection(element: GraphElement[SVGElement]): Unit = {
    element.selectElem.setAttribute("stroke", "#555555")
    element.selectElem.setAttribute("stroke-dasharray", "3 5")
  }

  def unsetSelection(element: GraphElement[SVGElement]): Unit = {
    element.selectElem.setAttribute("stroke", null)
    element.selectElem.setAttribute("stroke-dasharray", null)
  }

  def renderNode(id: String, shape: NodeShape, resources: Map[String, GraphMLResource]): SVGNodeElement = {
    val x = shape.geometry.map(_.x).getOrElse(0.0)
    val y = shape.geometry.map(_.y).getOrElse(0.0)

    val width = shape.geometry.map(_.width).getOrElse(80.0)
    val height = shape.geometry.map(_.height).getOrElse(50.0)

    val shapeElement = shape.svgContent.flatMap(content => resources.get(content.refId)) match {
      case Some(resource) if resource.typeHint.forall(_ == "svg") =>
        new DOMParser()
          .parseFromString(unescapeXml(resource.value), "application/xml")
          .firstChild
          .asInstanceOf[SVGElement]

      case _ => shapeTag(shape, width, height, resources)(
        svgAttrs.style := nodeStyle(shape).mkString(";"),
      ).render
    }

    val label: Text = shape.label
      .map(renderLabel(id, "node", _, width, height))
      .getOrElse(svg.text("").render)

    val selectRect = SVGGraphRenderer.renderSelectRect(x, y, width, height)
    selectRect.setAttribute("data-id", id)
    selectRect.setAttribute("data-type", "node")

    SVGNodeElement(
      id,
      shapeElement,
      label,
      selectRect,
      svg.g(
        svgAttrs.transform := s"translate($x $y)",
        data("type") := "node",
        data("id") := id,
        shapeElement,
        label
      ).render
    )
  }

  private def unescapeXml(xml: String): String = xml
    .replaceAll("&gt;", ">")
    .replaceAll("&lt;", "<")
    .replaceAll("&#13;", "")

  def shapeTag(shape: NodeShape,
               width: Double,
               height: Double,
               resources: Map[String, GraphMLResource]): JsDom.TypedTag[SVGGraphElement] = {
    lazy val fallback = svg.rect(
      svgAttrs.width :=  width,
      svgAttrs.height := height,
      svgAttrs.x := 0,
      svgAttrs.y := 0,
    )

    lazy val vectorShape = shape.shapeType match {
      case Some(ShapeType.Ellipse) => svg.ellipse(
        svgAttrs.cx := width / 2,
        svgAttrs.cy := height / 2,
        svgAttrs.rx := width / 2,
        svgAttrs.ry := height / 2
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

    shape.image.flatMap(img => resources.get(img.refId)) match {
      case Some(imageResource) if imageResource.typeHint.contains("java.awt.image.BufferedImage") =>
        svg.image(
          svgAttrs.width :=  width,
          svgAttrs.height := height,
          svgAttrs.xLinkHref := s"data:image/png;base64,${unescapeXml(imageResource.value).replaceAll("\n", "")}"
        )
      case Some(imageUrl) if imageUrl.typeHint.contains("dataUrl") => svg.image(
        svgAttrs.width :=  width,
        svgAttrs.height := height,
        svgAttrs.xLinkHref := imageUrl.value
      )
      case _ => vectorShape
    }
  }

  def nodeStyle(shape: NodeShape): Seq[String] = {
    val borderProps: Option[List[String]] = for {
      bs <- shape.borderStyle.orElse(Some(BorderStyle("#000000", styleType = "line", width = 1.0)))
    } yield List(
      s"stroke:${bs.color}",
      s"stroke-width:${bs.width}"
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
      cls := "draggable",
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
                  label: LabelLike,
                  width: Double,
                  height: Double): Text = {
    val lines = label.text.split("\n")

    val textCenterX = width / 2
    val textCenterY = (height / (1 + lines.size)) - Try(label.fontSize.getOrElse(defaultFontSize).toDouble * 0.6).getOrElse(10.0)
    val isFree = label.model.contains(Free)

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
        pos <- label.position
      } yield (pos.x, pos.y)).getOrElse((0.0, 0.0))
    } else (textCenterX, textCenterY)

    svg.text(
      svgAttrs.transform := s"translate($finalX $finalY)",
      svgAttrs.textAnchor := "start",
      svgAttrs.fontFamily := (if (label.fontFamily.contains("Dialog")) "Helvetica" else label.fontFamily.getOrElse(defaultFontFamily)), // Dialog does not exist in the most browser
      svgAttrs.fontSize := label.fontSize.getOrElse(defaultFontSize),
      svgAttrs.fill := label.textColor.getOrElse(defaultTextColor),
    )(spans: _*)
  }.render

  def renderEdge(edge: Edge[GraphMLEdge[Json]],
                 graphml: GraphMLGraph[Json, Json]): Option[SVGEdgeElement] = {
    for {
      points <- DrawUtil.getLinePoints(edge, graphml).map(_.toList)
      start <- points.headOption
      end <- points.reverse.headOption

      pointsString = points.map(p => s"${p.x} ${p.y}").mkString(", ")
      shape <- edge.value.shape.orElse(Some(EdgeShape()))

      line = {
        svg.polyline(
          svgAttrs.points := pointsString,
          svgAttrs.style := s"fill:none",
          svgAttrs.stroke := shape.edgeStyle.map(_.color).getOrElse("#000000"),
          svgAttrs.strokeWidth := shape.edgeStyle.flatMap(_.width).getOrElse(1.0),
          svgAttrs.markerStart := shape.arrows.flatMap(_.source).map(source => s"url(#arrow_${source})").getOrElse(""),
          svgAttrs.markerEnd :=  shape.arrows.flatMap(_.target).map(target => s"url(#arrow_${target})").getOrElse("")
        ).render
      }

      selectLine = {
        val polyline = svg.polyline(
          cls := "selectable",
          svgAttrs.points := pointsString,
          svgAttrs.style := s"stroke:#ffffff;stroke-width:15px;fill:none;cursor: pointer",
          svgAttrs.strokeOpacity := 0.0
        ).render

        polyline.setAttribute("data-id", edge.id)
        polyline.setAttribute("data-type", "edge")

        polyline
      }

      label =  {
        val bbox = line.getBBox()
        for {
          edgeLabel <- shape.label
          pos <- edgeLabel.position.orElse(Some(PointSpec((end.x - start.x) / 2, (end.y - start.y) / 2)))
          withStartPoint = edgeLabel.copy(position = Some(PointSpec(pos.x + start.x, pos.y + start.y)))
        } yield renderLabel(edge.id, "edge", withStartPoint, bbox.width, bbox.height)
      }.getOrElse(svg.text("").render)

      group = svg.g(id := edge.id, line, selectLine).render

    } yield SVGEdgeElement(edge.id, group, label, group)
  }

}
