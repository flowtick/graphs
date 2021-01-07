package com.flowtick.graphs

package object style {
  final case class ImageSpec(data: String, imageType: String, width: Option[Double] = None, height: Option[Double] = None)

  final case class Fill(color: Option[String], transparent: Option[Boolean] = None) extends FillLike

  sealed trait LabelModel
  case object Custom extends LabelModel
  case object Free extends LabelModel

  sealed trait LabelStyle {
    def textColor: Option[String]
    def fontSize: Option[String]
    def fontFamily: Option[String]
    def model: Option[LabelModel]
    def position: Option[PointSpec]
  }

  sealed trait FillLike {
    def color: Option[String]
  }

  final case class NodeLabel(textColor: Option[String] = None,
                             fontSize: Option[String] = None,
                             fontFamily: Option[String] = None,
                             modelName: Option[String] = None,
                             position: Option[PointSpec] = None) extends LabelStyle {
    override def model: Option[LabelModel] = modelName.map {
      case "custom" => Custom
      case _ => Free
    }
  }

  final case class BorderStyle(color: String, styleType: Option[String], width: Option[Double])
  final case class SVGContent(refId: String)

  object ShapeType {
    val Rectangle = "rectangle"
    val RoundRectangle = "roundrectangle"
    val Ellipse = "ellipse"
  }

  final case class NodeShape(fill: Option[Fill] = None,
                             labelStyle: Option[NodeLabel] = None,
                             shapeType: Option[String] = None,
                             borderStyle: Option[BorderStyle] = None,
                             image: Option[String] = None,
                             svgContent: Option[SVGContent] = None)

  final case class Arrows(source: Option[String], target: Option[String])
  final case class EdgeStyle(color: String, width: Option[Double] = None)
  final case class EdgeLabel(textColor: Option[String] = None,
                             fontSize: Option[String] = None,
                             fontFamily: Option[String] = None,
                             model: Option[LabelModel] = Some(Free),
                             position: Option[PointSpec] = None) extends LabelStyle

  final case class PointSpec(x: Double, y: Double)

  final case class EdgePath(sourceX: Double,
                            sourceY: Double,
                            targetX: Double,
                            targetY: Double,
                            points: List[PointSpec])

  final case class EdgeShape(labelStyle: Option[EdgeLabel] = None,
                             edgeStyle: Option[EdgeStyle] = None,
                             arrows: Option[Arrows] = None)
}
