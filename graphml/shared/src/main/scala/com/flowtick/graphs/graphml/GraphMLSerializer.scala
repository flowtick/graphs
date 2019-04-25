package com.flowtick.graphs.graphml

import com.flowtick.graphs.{Edge, Graph, Identifiable, Labeled}
import shapeless.ops.hlist.HKernelAux
import shapeless.{Generic, HList}

import scala.xml.{Node, NodeSeq, Text}

class GraphMLSerializer[V, N, M, Repr <: HList](implicit
  identifiable: Identifiable[GraphMLNode[N]],
  edgeLabel: Labeled[Edge[GraphMLEdge[V], GraphMLNode[N]], String],
  serializer: Serializer[GraphMLNode[N]], ker: HKernelAux[Repr]) extends Serializer[Graph[GraphMLEdge[V], GraphMLNode[N], GraphMLGraph[M]]] {
  override def serialize(
    g: Graph[GraphMLEdge[V], GraphMLNode[N], GraphMLGraph[M]]): NodeSeq = {

    ker().length

    def nodeDataKeys = Seq(GraphMLKey(id = "graphics", targetHint = Some("node"), yfilesType = Some("nodegraphics")))

    def dataKeys: Iterable[Node] = nodeDataKeys.map { key: GraphMLKey =>
      <key id={ key.id } for={ key.targetHint.map(Text(_)) } yfiles.type={ key.yfilesType.map(Text(_)) } attr.type={ key.typeHint.map(Text(_)) }/>
    }

    def edgesXml: Iterable[Node] = {
      g.edges.flatMap { edge =>
        val source = edge.head
        val target = edge.tail
        <edge id={ identifiable.id(source) + "-" + identifiable.id(target) } source={ identifiable.id(source) } target={ identifiable.id(target) }/>
      }
    }

    def nodesXml: Iterable[Node] = g.nodes.flatMap(serializer.serialize)

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
