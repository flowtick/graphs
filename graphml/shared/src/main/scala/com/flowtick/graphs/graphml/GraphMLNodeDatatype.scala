package com.flowtick.graphs.graphml

import cats.data.Validated._
import cats.data.ValidatedNel
import com.flowtick.graphs.layout.{ Geometry, ShapeDefinition }
import shapeless.ops.record.Keys
import shapeless._

import scala.reflect.ClassTag
import scala.xml.{ Elem, Node, NodeSeq }

/**
 * @param genericValue
 * @param fromList
 * @param genericValueKeys
 * @tparam T
 * @tparam Repr the representation as labelled generic
 * @tparam FromRepr the representation as generic
 *
 * see FromList to understand why we need two representations
 *
 * the good news is that an implicit LabelledGeneric will also be provide an implicit Generic
 *
 */
class GraphMLNodeDatatype[T, Repr <: HList, FromRepr <: HList](implicit
  genericValue: LabelledGeneric.Aux[T, Repr],
  fromList: FromList[T, FromRepr],
  genericValueKeys: Keys[Repr],
  classTag: ClassTag[T]) extends Datatype[GraphMLNode[T]] {
  private val valueKeys: List[String] = genericValueKeys().runtimeList.map(_.asInstanceOf[Symbol].name)

  override def keys: Seq[GraphMLKey] =
    valueKeys.map(keyName => GraphMLKey(id = s"node_$keyName", name = Some(keyName), targetHint = Some("node"), typeHint = Some("string"), graphsType = Some(classTag.toString()))) ++
      Seq(GraphMLKey(id = "node_graphics", targetHint = Some("node"), yfilesType = Some("nodegraphics")))

  def serialize(node: GraphMLNode[T]): NodeSeq = {
    val nodeValueData = genericValueKeys().runtimeList.zip(genericValue.to(node.value).runtimeList).map {
      case (key: Symbol, value) if classOf[Integer].isAssignableFrom(value.getClass) =>
        <data key={ s"node_${key.name}" } type="integer">{ value.toString }</data>

      case (key: Symbol, value) if classOf[Double].isAssignableFrom(value.getClass) =>
        <data key={ s"node_${key.name}" } type="double">{ value.toString }</data>

      case (key: Symbol, value) =>
        <data key={ s"node_${key.name}" } type="string">{ value.toString }</data>

      case (_, _) => <!-- unknown value -->
    }

    val nodePropertiesData = node.properties.map { property =>
      <data key={ property.key }>{ property.value.toString }</data>
    }

    // format: OFF
    Seq(
      <node id={ node.id }>
        { nodeValueData }
        { nodePropertiesData }
        <data key="node_graphics">
          {GraphMLNodeDatatype.shapeXml(node.label.orElse(Some(node.id)), node.geometry, node.shape)}
        </data>
      </node>
    )
    // format: ON
  }

  override def deserialize(from: NodeSeq, graphKeys: scala.collection.Map[String, GraphMLKey]): ValidatedNel[Throwable, GraphMLNode[T]] =
    from.headOption match {
      case Some(node) =>
        val id = GraphMLDatatype.singleAttributeValue("id", node).getOrElse(node.label)

        GraphMLDatatype.parseValue(node, graphKeys, valueKeys).map { value =>
          // we assume that we will always recreate the graphics from the domain
          // so we will not pass though the node graphics prop,
          // as it would be inconsistent and possible duplicated
          // TODO: parse graphics properties
          val nonGraphicProperties = value
            .properties
            .filterNot(prop => {
              graphKeys.get(prop.key).exists(_.yfilesType.exists(_ == "nodegraphics"))
            })

          GraphMLNode[T](id, value.value, extractNodeLabel(value.properties, graphKeys), nonGraphicProperties)
        }

      case None => invalidNel(new IllegalArgumentException(s"invalid node xml $from"))
    }

  protected def extractNodeLabel(properties: Seq[GraphMLProperty], keys: scala.collection.Map[String, GraphMLKey]): Option[String] = {
    properties.find(prop => keys.get(prop.key).exists(_.yfilesType.exists(_ == "nodegraphics"))).flatMap { nodeGraphics =>
      nodeGraphics.value match {
        case xml: Seq[scala.xml.Node @unchecked] =>
          val nodeLabel: Option[Node] = xml.foldLeft(Seq.empty[scala.xml.Node])((a, b) => a ++ b.nonEmptyChildren).find(_.label == "NodeLabel")
          nodeLabel.map(_.text.trim)
      }
    }
  }

}

object GraphMLNodeDatatype {
  def shapeXml(
    label: Option[String],
    geometry: Option[Geometry],
    shape: Option[ShapeDefinition]): Elem = {
    // format: OFF
    <y:ShapeNode>
      <y:Geometry height={ geometry.map(_.height).getOrElse(30).toString } width={ geometry.map(_.width).getOrElse(30).toString } x={ geometry.map(_.x).getOrElse(0).toString } y={ geometry.map(_.y).getOrElse(0).toString }/>
      <y:Fill color={ shape.map(_.color).getOrElse("#FFFFFF") } transparent="false"/>
      <y:BorderStyle color="#000000" raised="false" type="line" width="1.0"/>
      <y:NodeLabel alignment="center"
                   autoSizePolicy="content"
                   fontFamily="Dialog"
                   fontSize="12"
                   fontStyle="plain"
                   hasBackgroundColor="false"
                   hasLineColor="false"
                   horizontalTextPosition="center"
                   iconTextGap="4"
                   modelName="custom"
                   textColor="#000000"
                   verticalTextPosition="bottom" visible="true">{ scala.xml.PCData(label.getOrElse("")) }<y:LabelModel>
          <y:SmartNodeLabelModel distance="4.0"/>
        </y:LabelModel>
        <y:ModelParameter>
          <y:SmartNodeLabelModelParameter labelRatioX="0.0" labelRatioY="0.0" nodeRatioX="0.0" nodeRatioY="0.0" offsetX="0.0" offsetY="0.0" upX="0.0" upY="-1.0"/>
        </y:ModelParameter>
      </y:NodeLabel>
      <y:Shape type={ shape.map(_.shapeType).getOrElse("rectangle") }/>
    </y:ShapeNode>
    // format: ON
  }
}