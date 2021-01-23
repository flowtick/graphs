package com.flowtick.graphs

package object style {
  object ImageType {
    val url = "url"
    val svg = "svg"
    val dataUrl = "dataUrl"
  }

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
    def position: Option[StylePos]
    def border: Option[BorderStyle]
  }

  sealed trait FillLike {
    def color: Option[String]
  }

  final case class NodeLabel(textColor: Option[String] = None,
                             fontSize: Option[String] = None,
                             fontFamily: Option[String] = None,
                             modelName: Option[String] = None,
                             position: Option[StylePos] = None,
                             border: Option[BorderStyle] = None) extends LabelStyle {
    override def model: Option[LabelModel] = modelName.map {
      case "custom" => Custom
      case _ => Free
    }
  }

  final case class BorderStyle(color: String, styleType: Option[String] = None, width: Option[Double] = None)
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
                             position: Option[StylePos] = None,
                             border: Option[BorderStyle] = None) extends LabelStyle

  final case class StylePos(x: Double, y: Double)

  final case class EdgeShape(labelStyle: Option[EdgeLabel] = None,
                             edgeStyle: Option[EdgeStyle] = None,
                             arrows: Option[Arrows] = None)

  object defaults {
    implicit def generalNodeStyleRef[T]: StyleRef[Node[T]] = new StyleRef[Node[T]] {
      override def id(node: Node[T]): Option[String] = Some(node.id)
      override def classList(element: Node[T]): List[String] = List.empty
    }

    implicit def generalEdgeStyleRef[T]: StyleRef[Edge[T]] = new StyleRef[Edge[T]] {
      override def id(edge: Edge[T]): Option[String] = Some(edge.id)
      override def classList(element: Edge[T]): List[String] = List.empty
    }
  }

}
