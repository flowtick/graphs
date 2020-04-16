package com.flowtick.graphs.graphml

import cats.data.ValidatedNel
import cats.data.Validated._
import com.flowtick.graphs.Identifiable
import shapeless.{ HList, LabelledGeneric }
import shapeless.ops.record.Keys

import scala.reflect.ClassTag
import scala.xml.{ Node, NodeSeq }

class GraphMLEdgeDatatype[V, Repr <: HList, FromRepr <: HList](implicit
  identifiable: Identifiable[GraphMLNode[V], String],
  genericValue: LabelledGeneric.Aux[V, Repr],
  fromList: FromList[V, FromRepr],
  genericValueKeys: Keys[Repr],
  classTag: ClassTag[V]) extends Datatype[GraphMLEdge[V]] {
  private val valueKeys: List[String] = genericValueKeys().runtimeList.map(_.asInstanceOf[Symbol].name)

  override def keys: Seq[GraphMLKey] =
    valueKeys.map(keyName => GraphMLKey(id = s"edge_$keyName", name = Some(keyName), targetHint = Some("edge"), typeHint = Some("string"), graphsType = Some(classTag.toString()))) ++
      Seq(GraphMLKey(id = "edge_graphics", targetHint = Some("edge"), yfilesType = Some("edgegraphics")))

  override def serialize(edge: GraphMLEdge[V]): NodeSeq = {
    val sourceId = edge.source.getOrElse(s"${edge.id}-source")
    val targetId = edge.target.getOrElse(s"${edge.id}-target")
    val edgeId = s"$sourceId-$targetId"

    val edgeValueData = genericValueKeys()
      .runtimeList
      .zip(genericValue.to(edge.value).runtimeList)
      .map(value => {
        val (key, valueString, typeHint) = value match {
          case (key: Symbol, value) if classOf[Integer].isAssignableFrom(value.getClass) => (key.name, value.toString, "integer")
          case (key: Symbol, value) if classOf[Double].isAssignableFrom(value.getClass) => (key.name, value.toString, "double")
          case (key: Symbol, value) => (key.name, value.toString, "string")
        }

        <data key={ s"edge_${key}" } type={ typeHint }>{ valueString.toString }</data>
      })

    val edgePropertiesData = edge.properties.map { property =>
      <data key={ property.key } type={ property.typeHint.getOrElse("") }>{ property.value.toString }</data>
    }
    // format: OFF
    <edge id={ edgeId } source={ sourceId } target={ targetId }>
      { edgeValueData }
      { edgePropertiesData }
      <data key="edge_graphics">

        <y:PolyLineEdge>
          <y:Path sx="0.0" sy="0.0" tx="0.0" ty="0.0"/>
          <y:LineStyle color="#000000" type="line" width="1.0"/>
          <y:Arrows source="none" target="none"/>
          <y:EdgeLabel alignment="center" configuration="AutoFlippingLabel" distance="2.0" fontFamily="Dialog" fontSize="12" fontStyle="plain" hasBackgroundColor="false" hasLineColor="false" height="17.96875" horizontalTextPosition="center" iconTextGap="4" modelName="custom" preferredPlacement="anywhere" ratio="0.5" textColor="#000000" verticalTextPosition="bottom" visible="true" width="26.904296875" x="16.5478515625" y="-28.984375">{ edge.label.getOrElse("") }<y:LabelModel>
                                          <y:SmartEdgeLabelModel autoRotationEnabled="false" defaultAngle="0.0" defaultDistance="10.0"/>
                                        </y:LabelModel>
            <y:ModelParameter>
              <y:SmartEdgeLabelModelParameter angle="0.0" distance="30.0" distanceToCenter="true" position="right" ratio="0.5" segment="0"/>
            </y:ModelParameter>
            <y:PreferredPlacementDescriptor angle="0.0" angleOffsetOnRightSide="0" angleReference="absolute" angleRotationOnRightSide="co" distance="-1.0" frozen="true" placement="anywhere" side="anywhere" sideReference="relative_to_edge_flow"/>
          </y:EdgeLabel>
          <y:BendStyle smoothed="false"/>
        </y:PolyLineEdge>

      </data>
    </edge>
    // format: ON
  }

  override def deserialize(from: NodeSeq, graphKeys: collection.Map[String, GraphMLKey]): ValidatedNel[Throwable, GraphMLEdge[V]] =
    (for {
      edgeNode <- from.headOption
      edgeId <- GraphMLDatatype.singleAttributeValue("id", edgeNode).orElse(Some("edge"))
    } yield {
      val source = GraphMLDatatype.singleAttributeValue("source", edgeNode)
      val target = GraphMLDatatype.singleAttributeValue("target", edgeNode)

      GraphMLDatatype.parseValue(edgeNode, graphKeys, valueKeys).map { value =>
        // we assume that we will always recreate the graphics from the domain
        // so we will not pass though the node graphics prop,
        // as it would be inconsistent and possible duplicated
        // TODO: parse graphics properties
        val nonGraphicProperties = value
          .properties
          .filterNot(prop => {
            graphKeys.get(prop.key).exists(_.yfilesType.exists(_ == "edgegraphics"))
          })

        GraphMLEdge[V](
          edgeId,
          value.value,
          source,
          target,
          extractEdgeLabel(value.properties, graphKeys),
          nonGraphicProperties)
      }
    }).getOrElse(invalidNel(new IllegalArgumentException(s"unable to parse GraphML edge from ${from.toString}")))

  protected def extractEdgeLabel(properties: Seq[GraphMLProperty], keys: scala.collection.Map[String, GraphMLKey]): Option[String] = {
    properties.find(prop => keys.get(prop.key).exists(_.yfilesType.exists(_ == "edgegraphics"))).flatMap { edgeGraphics =>
      edgeGraphics.value match {
        case xml: Seq[scala.xml.Node @unchecked] =>
          val edgeLabel: Option[Node] = xml.foldLeft(Seq.empty[scala.xml.Node])((a, b) => a ++ b.nonEmptyChildren).find(_.label == "EdgeLabel")
          edgeLabel.map(_.text.trim)
      }
    }
  }
}
