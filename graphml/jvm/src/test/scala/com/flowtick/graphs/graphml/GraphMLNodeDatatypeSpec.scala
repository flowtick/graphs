package com.flowtick.graphs.graphml

import cats.data.Validated.Valid
import cats.data.ValidatedNel
import com.flowtick.graphs.layout.DefaultGeometry
import com.flowtick.graphs.style.{BorderStyle, Fill, NodeLabel, NodeShape}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.xml.NodeSeq
import generic._

case class SomeNodeValue(one: String, two: String)

class GraphMLNodeDatatypeSpec extends AnyFlatSpec with Matchers {
  def prettyXml(xml: scala.xml.Node) =
    new scala.xml.PrettyPrinter(80, 4).format(xml)

  it should "serialize a generic GraphML node" in {
    val fooDataType = GraphMLNodeDatatype[SomeNodeValue]

    val targetHint = Some("node")
    val nodeShape = NodeShape(
      fill = Some(Fill(color = Some("#FFFFFF"), transparent = Some(false))),
      borderStyle = Some(BorderStyle("#000000", Some("line"), Some(1.0))),
      labelStyle = Some(NodeLabel()),
      shapeType = Some("rectangle")
    )

    val serialized: NodeSeq = fooDataType.serialize(
      GraphMLNode(
        id = "test",
        value = SomeNodeValue("foo", "bar"),
        shape = Some(nodeShape),
        geometry = Some(DefaultGeometry(0.0, 0.0, 30.0, 30.0)),
        labelValue = Some("test")
      ),
      targetHint
    )

    val expectedXml = <node id="test">
                        <data key="node_one" type="string">foo</data>
                        <data key="node_two" type="string">bar</data>
                        <data key="node_graphics">
                          <y:ShapeNode>
                            <y:Geometry height="30.0" width="30.0" x="0.0" y="0.0"/>
                            <y:Fill hasColor="true" color="#FFFFFF" transparent="false"/>
                            <y:BorderStyle color="#000000" raised="false" type="line" width="1.0"/>
                            <y:NodeLabel alignment="center" autoSizePolicy="content" fontFamily="Dialog" fontSize="12" fontStyle="plain" hasBackgroundColor="false" hasLineColor="false" horizontalTextPosition="center" iconTextGap="4" modelName="custom" x="0.0" y="0.0" textColor="#000000" verticalTextPosition="bottom" visible="true">
                              <![CDATA[test]]><y:LabelModel><y:SmartNodeLabelModel distance="4.0"/></y:LabelModel>
                              <y:ModelParameter>
                                <y:SmartNodeLabelModelParameter labelRatioX="0.0" labelRatioY="0.0" nodeRatioX="0.0" nodeRatioY="0.0" offsetX="0.0" offsetY="0.0" upX="0.0" upY="-1.0"/>
                              </y:ModelParameter>
                            </y:NodeLabel>
                            <y:Shape type="rectangle"/>
                          </y:ShapeNode>
                        </data>
                      </node>

    serialized.headOption match {
      case Some(node) => prettyXml(node) should be(prettyXml(expectedXml))
      case _          => fail()
    }

    val deserialized: ValidatedNel[Throwable, GraphMLNode[SomeNodeValue]] =
      fooDataType.deserialize(
        serialized,
        fooDataType.keys(targetHint).map(key => (key.id, key)).toMap,
        targetHint
      )
    deserialized.map(_.value) should be(Valid(SomeNodeValue("foo", "bar")))
  }
}
