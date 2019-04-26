package com.flowtick.graphs.graphml

import cats.data.{ NonEmptyList, ValidatedNel }
import cats.data.Validated._
import com.flowtick.graphs.{ Edge, Graph, Identifiable, Labeled }
import xmls.XMLS

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.{ Either, Left, Right }
import scala.xml.{ Node, NodeSeq, Text }

class GraphMLDatatype[V, N, M](implicit
  identifiable: Identifiable[GraphMLNode[N]],
  edgeLabel: Labeled[Edge[GraphMLEdge[Unit], GraphMLNode[N]], String],
  nodeDataType: Datatype[GraphMLNode[N]]) extends Datatype[Graph[GraphMLEdge[Unit], GraphMLNode[N], GraphMLGraph[Unit]]] {
  override def serialize(g: Graph[GraphMLEdge[Unit], GraphMLNode[N], GraphMLGraph[Unit]]): NodeSeq = {

    def nodeKeys: Iterable[Node] = nodeDataType.keys.map { key: GraphMLKey =>
      // format: OFF
      <key id={ key.id }
           attr.name={ key.name.getOrElse(key.id) }
           for={ key.targetHint.map(Text(_)) }
           yfiles.type={ key.yfilesType.map(Text(_)) }
           attr.type={ key.typeHint.map(Text(_)) }/>
      // format: ON
    }

    def edgesXml: Iterable[Node] = {
      g.edges.flatMap { edge =>
        val source = edge.head
        val target = edge.tail
        <edge id={ identifiable.id(source) + "-" + identifiable.id(target) } source={ identifiable.id(source) } target={ identifiable.id(target) }/>
      }
    }

    def nodesXml: Iterable[Node] = g.nodes.flatMap(nodeDataType.serialize)

    // format: OFF
    <graphml xmlns="http://graphml.graphdrawing.org/xmlns" xmlns:java="http://www.yworks.com/xml/yfiles-common/1.0/java" xmlns:sys="http://www.yworks.com/xml/yfiles-common/markup/primitives/2.0" xmlns:x="http://www.yworks.com/xml/yfiles-common/markup/2.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:y="http://www.yworks.com/xml/graphml" xmlns:yed="http://www.yworks.com/xml/yed/3" xsi:schemaLocation="http://graphml.graphdrawing.org/xmlns http://www.yworks.com/xml/schema/graphml/1.1/ygraphml.xsd">
      <!-- Created by https://github.com/flowtick/graphs GraphML renderer -->
      { nodeKeys }
      <graph id="G" edgedefault="directed">
        { nodesXml }
        { edgesXml }
      </graph>
    </graphml>
    // format: ON
  }

  override def deserialize(from: NodeSeq): ValidatedNel[Throwable, Graph[GraphMLEdge[Unit], GraphMLNode[N], GraphMLGraph[Unit]]] = {
    def parseGraphML(root: scala.xml.Node): ValidatedNel[Throwable, Graph[GraphMLEdge[Unit], GraphMLNode[N], GraphMLGraph[Unit]]] =
      root.child.find(_.label == "graph").map { graphNode =>
        val keys = parseKeys(root)

        val edgeXmlNodes = new mutable.ListBuffer[scala.xml.Node]()
        val nodes: mutable.Map[String, GraphMLNode[N]] = parseGraphNodes(graphNode, keys, edgeXmlNodes)

        val edges: mutable.Seq[Edge[GraphMLEdge[Unit], GraphMLNode[N]]] = edgeXmlNodes.flatMap { edgeNode =>
          val edgeId = GraphMLDatatype.singleAttributeValue("id", edgeNode).getOrElse("edge")

          for {
            source <- GraphMLDatatype.singleAttributeValue("source", edgeNode)
            target <- GraphMLDatatype.singleAttributeValue("target", edgeNode)
            sourceNode <- nodes.get(source)
            targetNode <- nodes.get(target)
          } yield {
            val properties = GraphMLDatatype.parseProperties(edgeNode, keys)

            val mlEdge = GraphMLEdge(
              edgeId,
              (),
              extractEdgeLabel(properties),
              properties)

            Edge[GraphMLEdge[Unit], GraphMLNode[N]](mlEdge, sourceNode, targetNode)
          }
        }

        validNel(Graph.create(GraphMLGraph[Unit]((), GraphMLDatatype.singleAttributeValue("id", graphNode)), nodes.values, edges))
      }.getOrElse(invalidNel(new IllegalArgumentException("")))

    from.headOption match {
      case Some(root) => parseGraphML(root)
      case None => invalidNel(new IllegalArgumentException("cant parse empty xml"))
    }
  }

  def fromXml(graphml: String)(implicit identifiable: Identifiable[GraphMLNode[N]]): Either[NonEmptyList[Throwable], Graph[GraphMLEdge[Unit], GraphMLNode[N], GraphMLGraph[Unit]]] = {
    XMLS.parse(graphml) match {
      case Right(rootElem) if rootElem.label.toLowerCase == "graphml" => deserialize(Seq(rootElem)).toEither
      case Right(nonGraphMl) => Left(NonEmptyList.of(new IllegalArgumentException(s"parsed elem is not a graphml element: $nonGraphMl")))
      case Left(error) => Left(NonEmptyList.of(error))
    }
  }

  protected def parseKeys(rootElem: scala.xml.Node): Seq[GraphMLKey] = {
    rootElem.child.filter(_.label.toLowerCase == "key").flatMap { keyElem =>
      GraphMLDatatype.singleAttributeValue("id", keyElem).map(id => {
        GraphMLKey(
          id = id,
          name = GraphMLDatatype.singleAttributeValue("attr.name", keyElem),
          targetHint = GraphMLDatatype.singleAttributeValue("for", keyElem),
          typeHint = GraphMLDatatype.singleAttributeValue("attr.type", keyElem),
          yfilesType = GraphMLDatatype.singleAttributeValue("yfiles.type", keyElem))
      })
    }
  }

  protected def extractEdgeLabel(properties: Seq[GraphMLProperty]): Option[String] = {
    properties.find(_.key.yfilesType.exists(_ == "edgegraphics")).flatMap { edgeGraphics =>
      edgeGraphics.value match {
        case xml: Seq[scala.xml.Node @unchecked] =>
          val edgeLabel: Option[Node] = xml.foldLeft(Seq.empty[scala.xml.Node])((a, b) => a ++ b.nonEmptyChildren).find(_.label == "EdgeLabel")
          edgeLabel.map(_.text.trim)
      }
    }
  }

  protected def parseGraphNodes(
    graphNode: scala.xml.Node,
    keys: Seq[GraphMLKey],
    edgeXmlNodes: mutable.ListBuffer[scala.xml.Node]): mutable.Map[String, GraphMLNode[N]] = {
    val nodes = new mutable.HashMap[String, GraphMLNode[N]]()

    graphNode.child.zipWithIndex.foreach {
      case (node: scala.xml.Node, _: Int) if node.label == "node" =>
        nodeDataType.deserialize(Seq(node)).map { graphNode =>
          nodes.put(graphNode.id, graphNode)

          node.child.foreach {
            case child if child.label == "graph" => nodes ++= parseGraphNodes(child, keys, edgeXmlNodes)
            case _ =>
          }
        }

      case (edge: scala.xml.Node, _: Int) if edge.label == "edge" =>
        edgeXmlNodes.append(edge)

      case _ =>
    }
    nodes
  }

}

object GraphMLDatatype {
  protected[graphml] def singleAttributeValue(attributeName: String, node: scala.xml.Node): Option[String] = {
    node.attribute(attributeName).getOrElse(Seq.empty).headOption.map(_.text)
  }

  protected[graphml] def parseProperties(node: Node, keys: Seq[GraphMLKey]): Seq[GraphMLProperty] = {
    val properties = new ListBuffer[GraphMLProperty]()
    node.child.foreach {
      case data if data.label == "data" =>
        GraphMLDatatype.singleAttributeValue("key", data).foreach { keyId =>
          val value = if (data.child.exists(!_.isInstanceOf[Text])) data.child else data.child.text
          keys.find(_.id == keyId).foreach(key => properties += GraphMLProperty(key, value))
        }
      case _ =>
    }
    properties
  }
}
