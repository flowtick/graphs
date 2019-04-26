package com.flowtick.graphs.graphml

import cats.data.Validated._
import cats.data.ValidatedNel
import com.flowtick.graphs.Identifiable
import shapeless.ops.record.Keys
import shapeless._

import scala.util.Try
import scala.xml.{ Node, NodeBuffer, NodeSeq }

class GraphMLNodeDatatype[T, Repr <: HList](implicit
  identifiable: Identifiable[GraphMLNode[T]],
  genericValue: LabelledGeneric.Aux[T, Repr],
  genericValueKeys: Keys[Repr]) extends Datatype[GraphMLNode[T]] {
  override def keys: Seq[GraphMLKey] =
    genericValueKeys().runtimeList.map { case sym: Symbol => GraphMLKey(id = sym.name, targetHint = Some("node"), typeHint = Some("string")) } ++
      Seq(GraphMLKey(id = "graphics", targetHint = Some("node"), yfilesType = Some("nodegraphics")))

  def serialize(node: GraphMLNode[T]): NodeSeq = {

    val nodeData = genericValueKeys().runtimeList.zip(genericValue.to(node.value).runtimeList).map {
      case (key: Symbol, value) => <data key={ key.name }>{ value.toString }</data>
    }

    // format: OFF
    Seq(<node id={ identifiable.id(node) }>
      { nodeData }

      <data key="graphics">
        <y:ShapeNode>
          <y:Geometry height={ node.geometry.map(_.height).getOrElse(30).toString } width={ node.geometry.map(_.width).getOrElse(30).toString } x={ node.geometry.map(_.x).getOrElse(0).toString } y={ node.geometry.map(_.y).getOrElse(0).toString }/>
          <y:Fill color={ node.shape.map(_.color).getOrElse("#FFFFFF") } transparent="false"/>
          <y:BorderStyle color="#000000" raised="false" type="line" width="1.0"/>
          <y:NodeLabel alignment="center" autoSizePolicy="content" fontFamily="Dialog" fontSize="12" fontStyle="plain" hasBackgroundColor="false" hasLineColor="false" horizontalTextPosition="center" iconTextGap="4" modelName="custom" textColor="#000000" verticalTextPosition="bottom" visible="true">{ node.label.getOrElse(node.id) }<y:LabelModel><y:SmartNodeLabelModel distance="4.0"/></y:LabelModel>
            <y:ModelParameter>
              <y:SmartNodeLabelModelParameter labelRatioX="0.0" labelRatioY="0.0" nodeRatioX="0.0" nodeRatioY="0.0" offsetX="0.0" offsetY="0.0" upX="0.0" upY="-1.0"/>
            </y:ModelParameter>
          </y:NodeLabel>
          <y:Shape type={ node.shape.map(_.shapeType).getOrElse("rectangle") }/>
        </y:ShapeNode>
      </data>
    </node>)
    // format: ON
  }

  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  override def deserialize(from: NodeSeq): ValidatedNel[Throwable, GraphMLNode[T]] =
    from.headOption match {
      case Some(node) =>
        val id = GraphMLDatatype.singleAttributeValue("id", node).getOrElse(node.label)
        val nodeProperties = GraphMLDatatype.parseProperties(node, keys)

        Try {
          val valueList = nodeProperties.filterNot(_.key.yfilesType.isDefined).foldRight(HNil: HList) {
            case (GraphMLProperty(_, value: NodeBuffer), values) => values.::(value.mkString(""))
            case (GraphMLProperty(_, value: Any), values) => values.::(value)
          }.asInstanceOf[Repr]

          genericValue.from(valueList)
        }.map(value => validNel(GraphMLNode[T](id, value, extractNodeLabel(nodeProperties), nodeProperties))).getOrElse(invalidNel(new IllegalStateException(s"unable to create node representation from $nodeProperties")))

      case None => invalidNel(new IllegalArgumentException(s"invalid node xml $from"))
    }

  protected def extractNodeLabel(properties: Seq[GraphMLProperty]): Option[String] = {
    properties.find(_.key.yfilesType.exists(_ == "nodegraphics")).flatMap { nodeGraphics =>
      nodeGraphics.value match {
        case xml: Seq[scala.xml.Node @unchecked] =>
          val nodeLabel: Option[Node] = xml.foldLeft(Seq.empty[scala.xml.Node])((a, b) => a ++ b.nonEmptyChildren).find(_.label == "NodeLabel")
          nodeLabel.map(_.text.trim)
      }
    }
  }

}
