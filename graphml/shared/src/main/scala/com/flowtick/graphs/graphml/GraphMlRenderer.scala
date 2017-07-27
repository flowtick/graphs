package com.flowtick.graphs.graphml

import com.flowtick.graphs.{ Edge, Graph, Identifiable, Node }

import scala.xml.Elem

class GraphMlRenderer {
  def render[N <: Node, E <: Edge[N]](g: Graph[N, E])(implicit identifiable: Identifiable[N]): Elem = {
    // format: OFF
    <graphml xmlns="http://graphml.graphdrawing.org/xmlns" xmlns:java="http://www.yworks.com/xml/yfiles-common/1.0/java" xmlns:sys="http://www.yworks.com/xml/yfiles-common/markup/primitives/2.0" xmlns:x="http://www.yworks.com/xml/yfiles-common/markup/2.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:y="http://www.yworks.com/xml/graphml" xmlns:yed="http://www.yworks.com/xml/yed/3" xsi:schemaLocation="http://graphml.graphdrawing.org/xmlns http://www.yworks.com/xml/schema/graphml/1.1/ygraphml.xsd">
      <!-- Created by graphs graphml renderer -->
      <key for="node" id="graphics" yfiles.type="nodegraphics"/>
      <graph id="G" edgedefault="directed">
        {
          g.nodes.map { node =>
            <node id={ identifiable.id(node) }>
              <data key="graphics">
                <y:ShapeNode>
                  <y:Geometry height="30.0" width="30.0" x="0" y="0"/>
                  <y:Fill color="#FFFFFF" transparent="false"/>
                  <y:BorderStyle color="#000000" raised="false" type="line" width="1.0"/>
                  <y:NodeLabel alignment="center"
                               autoSizePolicy="content"
                               fontFamily="Dialog" fontSize="12"
                               fontStyle="plain" hasBackgroundColor="false"
                               hasLineColor="false"
                               horizontalTextPosition="center"
                               iconTextGap="4"
                               modelName="custom"
                               textColor="#000000"
                               verticalTextPosition="bottom"
                               visible="true">{identifiable.id(node)}<y:LabelModel><y:SmartNodeLabelModel distance="4.0"/></y:LabelModel>
                    <y:ModelParameter>
                      <y:SmartNodeLabelModelParameter labelRatioX="0.0" labelRatioY="0.0" nodeRatioX="0.0" nodeRatioY="0.0" offsetX="0.0" offsetY="0.0" upX="0.0" upY="-1.0"/>
                    </y:ModelParameter>
                  </y:NodeLabel>
                  <y:Shape type="rectangle"/>
                </y:ShapeNode>
              </data>
            </node>
          }
        }
        {
          g.edges.map { edge =>
            <edge id={ identifiable.id(edge.source) + "-" + identifiable.id(edge.target) } source={ identifiable.id(edge.source) } target={ identifiable.id(edge.target) }/>
          }
        }
      </graph>
    </graphml>
    // format: ON
  }
}