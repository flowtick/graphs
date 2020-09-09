package com.flowtick.graphs.graphml

import cats.data.ValidatedNel
import cats.data.Validated._
import com.flowtick.graphs.graphml.GraphMLDatatype.isValueProperty

import scala.xml.{Node, NodeSeq}

class GraphMLEdgeDatatype[T](edgeDatatype: Datatype[T]) extends Datatype[GraphMLEdge[T]] {

  override def keys(targetHint: Option[String]): Seq[GraphMLKey] =
    edgeDatatype.keys(targetHint) ++ Seq(GraphMLKey(id = "edge_graphics", targetHint = targetHint, yfilesType = Some("edgegraphics")))

  override def serialize(edge: GraphMLEdge[T], targetHint: Option[String]): NodeSeq = {
    val sourceId = edge.source.getOrElse(s"${edge.id}-source")
    val targetId = edge.target.getOrElse(s"${edge.id}-target")
    val edgeId = s"$sourceId-$targetId"

    val edgeValueData = edgeDatatype.serialize(edge.value, targetHint)

    // format: OFF
    <edge id={ edgeId } source={ sourceId } target={ targetId }>
      { edgeValueData }
      <data key="edge_graphics">
        <y:PolyLineEdge>
          {
            (for {
              shape <- edge.shape
              path <- shape.path
            } yield <y:Path sx={path.sourceX.toString} sy={path.sourceY.toString} tx={path.targetX.toString} ty={path.targetY.toString}>
                      {path.points.map(point => <y:Point x={point.x.toString} y={point.y.toString}/>)}
                    </y:Path>).getOrElse(<!-- no path defined -->)
          }

          {
            (for {
              shape <- edge.shape
              style <- shape.edgeStyle
            } yield <y:LineStyle color={style.color} type="line" width={style.width.map(_.toString).orNull}/>).getOrElse(<!-- no style defined -->)
          }

          {
            (for {
              shape <- edge.shape
              arrows <- shape.arrows
            } yield <y:Arrows source={arrows.source.orNull} target={arrows.target.orNull}/>).getOrElse(<!-- no arrows defined -->)
          }

          {
          (for {
            shape <- edge.shape
            label <- shape.label
            position <- label.position.orElse(Some(PointSpec(0.0, 0.0)))
          } yield <y:EdgeLabel alignment="center"
                               configuration="AutoFlippingLabel"
                               distance="2.0"
                               fontFamily={label.fontFamily.getOrElse("Dialog")}
                               fontSize={label.fontSize.getOrElse("12")}
                               fontStyle="plain"
                               hasBackgroundColor="false"
                               hasLineColor="false"
                               height="15"
                               horizontalTextPosition="center"
                               iconTextGap="4"
                               modelName="custom"
                               preferredPlacement="anywhere"
                               ratio="0.5"
                               textColor="#000000"
                               verticalTextPosition="bottom"
                               visible="true"
                               width="30"
                               x={position.x.toString}
                               y={position.y.toString}>{ label.text }<y:LabelModel>
              <y:SmartEdgeLabelModel autoRotationEnabled="false" defaultAngle="0.0" defaultDistance="10.0"/>
            </y:LabelModel>
            <y:ModelParameter>
              <y:SmartEdgeLabelModelParameter angle="0.0" distance="30.0" distanceToCenter="true" position="right" ratio="0.5" segment="0"/>
            </y:ModelParameter>
            <y:PreferredPlacementDescriptor angle="0.0" angleOffsetOnRightSide="0" angleReference="absolute" angleRotationOnRightSide="co" distance="-1.0" frozen="true" placement="anywhere" side="anywhere" sideReference="relative_to_edge_flow"/>
          </y:EdgeLabel>).getOrElse(<!-- no arrows defined -->)
          }

          <y:BendStyle smoothed="false"/>
        </y:PolyLineEdge>
      </data>
    </edge>
    // format: ON
  }

  final case class EdgeValues(shape: Option[EdgeShape] = None,
                              valueXml: List[Node] = List.empty)

  override def deserialize(from: NodeSeq, graphKeys: collection.Map[String, GraphMLKey], targetHint: Option[String]): ValidatedNel[Throwable, GraphMLEdge[T]] =
    (for {
      edgeNode <- from.headOption
    } yield {
      val source = GraphMLDatatype.singleAttributeValue("source", edgeNode)
      val target = GraphMLDatatype.singleAttributeValue("target", edgeNode)

      val properties = GraphMLDatatype.parseProperties(edgeNode).toList

      val edgeValues = properties.foldLeft(EdgeValues()) {
        case (edge, property) if graphKeys.get(property.key).exists(_.yfilesType.exists(_ == "edgegraphics")) =>
          edge.copy(shape = extractEdgeShape(property))
        case (edge, property) if isValueProperty(property, graphKeys, edgeDatatype.keys(Some("edge")).map(_.id)) =>
          edge.copy(valueXml = property.xml :: edge.valueXml)
        case (edge, _) => edge
      }

      GraphMLDatatype.singleAttributeValue("id", edgeNode) match {
        case Some(id) =>
          edgeDatatype.deserialize(edgeValues.valueXml.reverse, graphKeys, targetHint).map { value =>
            GraphMLEdge[T](
              id,
              value,
              source,
              target,
              shape = edgeValues.shape)
          }
        case None => invalidNel(new IllegalArgumentException(s"unable to parse id from edge ${from.toString}"))
      }
    }).getOrElse(invalidNel(new IllegalArgumentException(s"unable to parse GraphML edge from ${from.toString}")))

  /**
   *
   * @param property
   * @return
   */

  protected def extractEdgeShape(property: GraphMLProperty): Option[EdgeShape] = property.xml
    .nonEmptyChildren
    .headOption
    .filter(_.label == "PolyLineEdge")
    .map { dataNode =>
      dataNode.child.foldLeft(EdgeShape()) {
        case (shape, elem) if elem.label == "EdgeLabel" =>
          val label = EdgeLabel(
            elem.text.trim,
            GraphMLDatatype.singleAttributeValue("textColor", elem),
            GraphMLDatatype.singleAttributeValue("fontSize", elem),
            GraphMLDatatype.singleAttributeValue("fontFamily", elem),
            model = Some(Free),
            for {
              x <- GraphMLDatatype.singleAttributeValue("x", elem).map(_.toDouble).orElse(Some(0.0))
              y <- GraphMLDatatype.singleAttributeValue("y", elem).map(_.toDouble).orElse(Some(0.0))
            } yield PointSpec(x, y)
          )
          shape.copy(label = Some(label))

        case (shape, elem) if elem.label == "LineStyle" =>
          val edgeStyle = for {
            color <- GraphMLDatatype.singleAttributeValue("color", elem)
          } yield EdgeStyle(color, GraphMLDatatype.singleAttributeValue("width", elem).map(_.toDouble))
          shape.copy(edgeStyle = edgeStyle)

        case (shape, elem) if elem.label == "Arrows" =>
          shape.copy(arrows = Some(Arrows(
            GraphMLDatatype.singleAttributeValue("source", elem),
            GraphMLDatatype.singleAttributeValue("target", elem)
          )))

        case (shape, elem) if elem.label == "Path" =>
          val path = for {
            sx <- GraphMLDatatype.singleAttributeValue("sx", elem)
            sy <- GraphMLDatatype.singleAttributeValue("sy", elem)
            tx <- GraphMLDatatype.singleAttributeValue("tx", elem)
            ty <- GraphMLDatatype.singleAttributeValue("ty", elem)
            points = elem.child.filter(_.label == "Point").flatMap { pointNode =>
              for {
                sx <- GraphMLDatatype.singleAttributeValue("x", pointNode)
                sy <- GraphMLDatatype.singleAttributeValue("y", pointNode)
              } yield PointSpec(sx.toDouble, sy.toDouble)
            }.toList
          } yield EdgePath(sx.toDouble, sy.toDouble, tx.toDouble, ty.toDouble, points)
          shape.copy(path = path)

        case (shape, _) => shape
      }
    }
}
