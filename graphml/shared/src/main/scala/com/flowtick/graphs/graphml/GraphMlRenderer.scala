package com.flowtick.graphs.graphml

import com.flowtick.graphs.{ Edge, Graph, Identifiable, Node }

import scala.xml.Elem

class GraphMlRenderer {
  def render[N <: Node, E <: Edge[N]](g: Graph[N, E])(implicit identifiable: Identifiable[N]): Elem = {
    <graphml xmlns="http://graphml.graphdrawing.org/xmlns" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd">
      <graph id="G" edgedefault="directed">
        {
          g.nodes.map { node =>
            <node id={ identifiable.id(node) }></node>
          }
        }
        {
          g.edges.map { edge =>
            <edge id={ identifiable.id(edge.source) + "-" + identifiable.id(edge.target) } source={ identifiable.id(edge.source) } target={ identifiable.id(edge.target) }/>
          }
        }
      </graph>
    </graphml>
  }
}
