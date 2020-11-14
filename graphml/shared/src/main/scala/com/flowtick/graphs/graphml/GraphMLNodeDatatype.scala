package com.flowtick.graphs.graphml

import cats.data.Validated._
import cats.data.ValidatedNel
import com.flowtick.graphs.graphml.GraphMLDatatype.isValueProperty
import com.flowtick.graphs.layout.{DefaultGeometry, Geometry}
import com.flowtick.graphs.style._

import scala.xml.{Elem, Node, NodeSeq}

case class GraphMLShape(nodeShape: Option[NodeShape], geometry: Option[Geometry], labelValue: Option[String])

class GraphMLNodeDatatype[T](nodeValueDatatype: Datatype[T]) extends Datatype[GraphMLNode[T]] {
  override def keys(targetHint: Option[String]): Seq[GraphMLKey] =
    nodeValueDatatype.keys(targetHint) ++ Seq(GraphMLKey(id = "node_graphics", targetHint = targetHint, yfilesType = Some("nodegraphics")))

  def serialize(node: GraphMLNode[T], targetHint: Option[String]): NodeSeq = {
    // format: OFF
    Seq(
      <node id={ node.id }>
        { nodeValueDatatype.serialize(node.value, targetHint) }
        <data key="node_graphics">
          {GraphMLNodeDatatype.shapeXml(node)}
        </data>
      </node>
    )
    // format: ON
  }

  override def deserialize(from: NodeSeq,
                           graphKeys: scala.collection.Map[String, GraphMLKey],
                           targetHint: Option[String]): ValidatedNel[Throwable, GraphMLNode[T]] = from.headOption.map { node =>
    val id = GraphMLDatatype.singleAttributeValue("id", node).getOrElse(node.label)
    var mlShape: Option[GraphMLShape] = None
    val valuesXml = scala.collection.mutable.ListBuffer[Node]()

    GraphMLDatatype
      .parseProperties(node)
      .foreach {
        case property if graphKeys.get(property.key).exists(_.yfilesType.exists(_ == "nodegraphics")) =>
          mlShape = extractNodeShape(property)
        case property if isValueProperty(property, graphKeys, nodeValueDatatype.keys(targetHint).map(_.id)) =>
          valuesXml += property.xml
        case _ =>
      }

    nodeValueDatatype
      .deserialize(valuesXml, graphKeys, targetHint)
      .map(value => GraphMLNode[T](id, value, mlShape.flatMap(_.nodeShape), mlShape.flatMap(_.geometry), mlShape.flatMap(_.labelValue)))
  }.getOrElse(invalidNel(new IllegalArgumentException("no xml given")))

  protected def extractNodeShape(property: GraphMLProperty): Option[GraphMLShape] = property.xml.child
    .headOption
    .map { dataNode =>
    dataNode.nonEmptyChildren.foldLeft(GraphMLShape(Some(NodeShape()), None, None)) {
      case (shape, elem) if elem.label == "Geometry" =>
        val geo = for {
          x <- GraphMLDatatype.singleAttributeValue("x", elem)
          y <- GraphMLDatatype.singleAttributeValue("y", elem)
          width <- GraphMLDatatype.singleAttributeValue("width", elem)
          height <- GraphMLDatatype.singleAttributeValue("height", elem)
        } yield DefaultGeometry(x.toDouble, y.toDouble, width.toDouble, height.toDouble)
        shape.copy(geometry = geo)

      case (shape, elem) if elem.label == "NodeLabel" =>
        val label = NodeLabel(
          GraphMLDatatype.singleAttributeValue("textColor", elem),
          GraphMLDatatype.singleAttributeValue("fontSize", elem),
          GraphMLDatatype.singleAttributeValue("fontFamily", elem),
          GraphMLDatatype.singleAttributeValue("modelName", elem),
          for {
            x <- GraphMLDatatype.singleAttributeValue("x", elem).map(_.toDouble).orElse(Some(0.0))
            y <- GraphMLDatatype.singleAttributeValue("y", elem).map(_.toDouble).orElse(Some(0.0))
          } yield PointSpec(x, y)
        )
        shape.copy(nodeShape = shape.nodeShape.map(_.copy(labelStyle = Some(label))), labelValue = Option(elem.text.trim))

      case (shape, elem) if elem.label == "Fill" =>
        val fill = for {
          color <- GraphMLDatatype.singleAttributeValue("color", elem).orElse(Some("#FFFFFF"))
          hasColor <- GraphMLDatatype.singleAttributeValue("hasColor", elem).orElse(Some("true"))
          transparent = GraphMLDatatype.singleAttributeValue("transparent", elem)
        } yield Fill(if (hasColor.toBoolean) Some(color) else None, transparent.map(_.toBoolean))
        shape.copy(nodeShape = shape.nodeShape.map(_.copy(fill = fill)))

      case (shape, elem) if elem.label == "Shape" =>
        shape.copy(nodeShape = shape.nodeShape.map(_.copy(shapeType = GraphMLDatatype.singleAttributeValue("type", elem))))

      case (shape, elem) if elem.label == "BorderStyle" =>
        val borderStyle = for {
          color <- GraphMLDatatype.singleAttributeValue("color", elem)
          styleType = GraphMLDatatype.singleAttributeValue("type", elem)
          width = GraphMLDatatype.singleAttributeValue("width", elem)
        } yield BorderStyle(color, styleType, width.map(_.toDouble))
        shape.copy(nodeShape = shape.nodeShape.map(_.copy(borderStyle = borderStyle)))

      case (shape, elem) if elem.label == "SVGModel" =>
        val svgContent = for {
          contentChild <- elem.nonEmptyChildren.headOption.filter(_.label == "SVGContent")
          refId <- GraphMLDatatype.singleAttributeValue("refid", contentChild)
        } yield SVGContent(refId)
        shape.copy(nodeShape = shape.nodeShape.map(_.copy(svgContent = svgContent)))

      case (shape, elem) if elem.label == "Image" =>
        shape.copy(nodeShape = shape.nodeShape.map(_.copy(image = GraphMLDatatype.singleAttributeValue("refid", elem))))

      case (shape, _) => shape
    }
  }

}

object GraphMLNodeDatatype {
  def apply[T](implicit nodeDataType: Datatype[T]) = new GraphMLNodeDatatype[T](nodeDataType)

  def shapeXml(node: GraphMLNode[_]): Elem = {
    // format: OFF
    <y:ShapeNode>
      {
        node
          .geometry
          .map(geometry => <y:Geometry height={ geometry.height.toString } width={ geometry.width.toString } x={ geometry.x.toString } y={ geometry.y.toString }/>)
          .getOrElse(<!-- no geometry defined -->)
      }

      {
      node.shape
        .flatMap(_.fill)
        .map(fill => <y:Fill hasColor={fill.color.isDefined.toString} color={ fill.color.getOrElse("#FFFFFF") } transparent={fill.transparent.getOrElse(false).toString}/>)
        .getOrElse(<!-- no fill defined -->)
      }

      {
      node.shape
        .flatMap(_.borderStyle)
        .map(borderStyle => <y:BorderStyle color={borderStyle.color} raised="false" type={borderStyle.styleType.getOrElse("line")} width={borderStyle.width.getOrElse(0.0).toString}/>)
        .getOrElse(<!-- no border style defined -->)
      }

      {
      node.shape
        .flatMap(_.labelStyle)
        .map(label => <y:NodeLabel alignment="center"
                                   autoSizePolicy="content"
                                   fontFamily={label.fontFamily.getOrElse("Dialog")}
                                   fontSize={label.fontSize.getOrElse("12")}
                                   fontStyle="plain"
                                   hasBackgroundColor="false"
                                   hasLineColor="false"
                                   horizontalTextPosition="center"
                                   iconTextGap="4"
                                   modelName="custom"
                                   x={label.position.map(_.x).getOrElse(0.0).toString}
                                   y={label.position.map(_.y).getOrElse(0.0).toString}
                                   textColor={label.textColor.getOrElse("#000000")}
                                   verticalTextPosition="bottom"
                                   visible="true">{ scala.xml.PCData(node.labelValue.getOrElse("")) }<y:LabelModel>
          <y:SmartNodeLabelModel distance="4.0"/>
        </y:LabelModel>
          <y:ModelParameter>
            <y:SmartNodeLabelModelParameter labelRatioX="0.0" labelRatioY="0.0" nodeRatioX="0.0" nodeRatioY="0.0" offsetX="0.0" offsetY="0.0" upX="0.0" upY="-1.0"/>
          </y:ModelParameter>
        </y:NodeLabel>)
        .getOrElse(<!-- no label defined -->)
      }

      {
      node.shape
        .flatMap(_.shapeType)
        .map(shapeType => <y:Shape type={ shapeType }/>)
        .getOrElse(<!-- no shape type defined -->)
      }

    </y:ShapeNode>
    // format: ON
  }
}