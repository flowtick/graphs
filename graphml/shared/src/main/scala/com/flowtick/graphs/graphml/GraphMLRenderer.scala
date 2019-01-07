package com.flowtick.graphs.graphml

import com.flowtick.graphs.layout.{ DefaultGeometry, GraphLayout, ShapeDefinition }
import com.flowtick.graphs.{ Graph, Identifiable, Labeled }

import scala.xml.{ Elem, Text }

class GraphMLRenderer {
  def render[N, E](
    g: Graph[N, E],
    layouter: GraphLayout,
    shapeDefinition: N => Option[ShapeDefinition] = (_: N) => None)(implicit identifiable: Identifiable[N], edgeLabel: Labeled[E, String]): Elem = {
    val layout = layouter.layout(g, shapeDefinition)

    def nodeProperties(aNode: N): Map[String, GraphMLProperty] = (aNode match {
      case GraphMLNode(_, _, properties) => properties
      case _ => Map.empty[String, GraphMLProperty]
    }) + ("graphics" -> nodeGraphicsProperty(aNode, identifiable.id(aNode), shapeDefinition(aNode)))

    def dataKeys: Set[Elem] = g.nodes.flatMap(nodeProperties(_).values).map { property: GraphMLProperty =>
      <key id={ property.key.id } for={ property.key.targetHint.map(Text(_)) } yfiles.type={ property.key.yfilesType.map(Text(_)) } attr.type={ property.key.typeHint.map(Text(_)) }/>
    }

    def dataValues(node: N) = nodeProperties(node).map {
      case (key, property) => <data key={ key }>{ property.value }</data>
    }

    def nodeGraphicsProperty(
      node: N,
      label: String,
      shape: Option[ShapeDefinition]): GraphMLProperty = {
      val geometry = layout(node).map(_.geometry).getOrElse(DefaultGeometry(0, 0, 30, 30))

      val shapeNodeElem =
        // format: OFF
        <y:ShapeNode>
          <y:Geometry height={geometry.height.toString}
                      width={geometry.width.toString}
                      x={geometry.x.toString}
                      y={geometry.y.toString}/>
          <y:Fill color={shape.map(_.color).getOrElse("#FFFFFF")} transparent="false"/>
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
                       verticalTextPosition="bottom"
                       visible="true">{label}<y:LabelModel><y:SmartNodeLabelModel distance="4.0"/></y:LabelModel>
            <y:ModelParameter>
              <y:SmartNodeLabelModelParameter labelRatioX="0.0" labelRatioY="0.0" nodeRatioX="0.0" nodeRatioY="0.0" offsetX="0.0" offsetY="0.0" upX="0.0" upY="-1.0"/>
            </y:ModelParameter>
          </y:NodeLabel>
          <y:Shape type={shape.map(_.shapeType).map {
          case "rectangle" if shape.exists(_.rounded) => "roundrectangle"
          case other@_ => other
        }.getOrElse("rectangle")}/>
        </y:ShapeNode>
      // format: ON

      GraphMLProperty(
        key = GraphMLKey(id = "graphics", targetHint = Some("node"), yfilesType = Some("nodegraphics")),
        value = shapeNodeElem)
    }

    def edgesXml = {
      g.edges.flatMap { edge =>
        for {
          source <- edge.predecessors.headOption
          target <- edge.successors.headOption
        } yield <edge id={ identifiable.id(source) + "-" + identifiable.id(target) } source={ identifiable.id(source) } target={ identifiable.id(target) }/>
      }
    }

    def nodesXml =
      g.nodes.map { node =>
        <node id={ identifiable.id(node) }>
          { dataValues(node) }
        </node>
      }

    // format: OFF
    <graphml xmlns="http://graphml.graphdrawing.org/xmlns" xmlns:java="http://www.yworks.com/xml/yfiles-common/1.0/java" xmlns:sys="http://www.yworks.com/xml/yfiles-common/markup/primitives/2.0" xmlns:x="http://www.yworks.com/xml/yfiles-common/markup/2.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:y="http://www.yworks.com/xml/graphml" xmlns:yed="http://www.yworks.com/xml/yed/3" xsi:schemaLocation="http://graphml.graphdrawing.org/xmlns http://www.yworks.com/xml/schema/graphml/1.1/ygraphml.xsd">
      <!-- Created by https://github.com/flowtick/graphs GraphML renderer -->
      { dataKeys }
      <graph id="G" edgedefault="directed">
        { nodesXml }
        { edgesXml }
      </graph>
    </graphml>
    // format: ON
  }

}
