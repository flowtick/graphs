package com.flowtick.graphs.graphml

import cats.Semigroup
import cats.implicits._
import cats.syntax.all._
import cats.data.{ NonEmptyList, ValidatedNel }
import cats.data.Validated
import cats.data.Validated._
import com.flowtick.graphs.{ Edge, Graph, Identifiable, Labeled }
import scala.xml.{ Node, NodeSeq, Text }

private[graphml] case class PartialParsedGraph[N](
  id: Option[String],
  nodes: scala.collection.Map[String, GraphMLNode[N]],
  edgesXml: Seq[scala.xml.Node])

private[graphml] object PartialParsedGraph {
  def empty[N]: PartialParsedGraph[N] = PartialParsedGraph[N](None, Map.empty, Seq.empty)

  implicit def partialGraphSemiGroup[N]: Semigroup[PartialParsedGraph[N]] = new Semigroup[PartialParsedGraph[N]] {
    override def combine(x: PartialParsedGraph[N], y: PartialParsedGraph[N]): PartialParsedGraph[N] =
      PartialParsedGraph(None, x.nodes ++ y.nodes, x.edgesXml ++ y.edgesXml)
  }
}

class GraphMLDatatype[V, N, M](implicit
  identifiable: Identifiable[GraphMLNode[N]],
  edgeLabel: Labeled[Edge[GraphMLEdge[Unit], GraphMLNode[N]], String],
  nodeDataType: Datatype[GraphMLNode[N]],
  edgeDataType: Datatype[GraphMLEdge[V]]) extends Datatype[Graph[GraphMLEdge[V], GraphMLNode[N], GraphMLGraph[Unit]]] {

  override def serialize(g: Graph[GraphMLEdge[V], GraphMLNode[N], GraphMLGraph[Unit]]): NodeSeq = {

    def nodeKeys: Iterable[Node] = g.value.keys.map { key: GraphMLKey =>
      // format: OFF
      <key id={ key.id }
           attr.name={ key.name.getOrElse(key.id) }
           for={ key.targetHint.map(Text(_)) }
           yfiles.type={ key.yfilesType.map(Text(_)) }
           attr.type={ key.typeHint.map(Text(_)) }
           graphs.type={key.graphsType.map(Text(_))} />
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

  override def deserialize(from: NodeSeq,
                           graphKeys: scala.collection.Map[String, GraphMLKey]): ValidatedNel[Throwable, Graph[GraphMLEdge[V], GraphMLNode[N], GraphMLGraph[Unit]]] = {
    def parseGraphML(root: scala.xml.Node): ValidatedNel[Throwable, Graph[GraphMLEdge[V], GraphMLNode[N], GraphMLGraph[Unit]]] =
      root.child.find(_.label == "graph").map { graphNode =>
        parseGraphNodes(graphNode, graphKeys: scala.collection.Map[String, GraphMLKey]).map { parsedGraph =>
          Graph.create(GraphMLGraph[Unit]((), parsedGraph.id, graphKeys.values.toSeq), parsedGraph.nodes.values, parseEdges(parsedGraph.edgesXml, parsedGraph.nodes, graphKeys))
        }
      }.getOrElse(invalidNel(new IllegalArgumentException("")))

    from.headOption match {
      case Some(root) => parseGraphML(root)
      case None => invalidNel(new IllegalArgumentException("cant parse empty xml"))
    }
  }

  protected def parseEdges(
    edgeXmlNodes: Seq[scala.xml.Node],
    nodes: scala.collection.Map[String, GraphMLNode[N]],
    keys: scala.collection.Map[String, GraphMLKey]): Seq[Edge[GraphMLEdge[V], GraphMLNode[N]]] =
    edgeXmlNodes.flatMap { edgeNode =>
      val edgeId = GraphMLDatatype.singleAttributeValue("id", edgeNode).getOrElse("edge")

      edgeDataType.deserialize(edgeNode.child, keys).map { edgeValue =>
        for {
          source <- GraphMLDatatype.singleAttributeValue("source", edgeNode)
          target <- GraphMLDatatype.singleAttributeValue("target", edgeNode)
          sourceNode <- nodes.get(source)
          targetNode <- nodes.get(target)
        } yield {
          val properties = GraphMLDatatype.parseProperties(edgeNode)

          val mlEdge = GraphMLEdge(
            edgeId,
            ,
            extractEdgeLabel(properties, keys),
            properties)

          Edge[GraphMLEdge[V], GraphMLNode[N]](edgeValue, sourceNode, targetNode)
        }
      }.orElse(invalidNel())

    }

  protected def extractEdgeLabel(properties: Seq[GraphMLProperty], keys: scala.collection.Map[String, GraphMLKey]): Option[String] = {
    properties.find(prop => keys.get(prop.key).exists(_.yfilesType.exists(_ == "edgegraphics"))).flatMap { edgeGraphics =>
      edgeGraphics.value match {
        case xml: Seq[scala.xml.Node @unchecked] =>
          val edgeLabel: Option[Node] = xml.foldLeft(Seq.empty[scala.xml.Node])((a, b) => a ++ b.nonEmptyChildren).find(_.label == "EdgeLabel")
          edgeLabel.map(_.text.trim)
      }
    }
  }

  def mergePartialGraphs(graphs: Seq[ValidatedNel[Throwable, PartialParsedGraph[N]]]): ValidatedNel[Throwable, PartialParsedGraph[N]] =
    graphs.foldLeft(validNel[Throwable, PartialParsedGraph[N]](PartialParsedGraph.empty[N]))(_ combine _)

  def parseNode(nodeXml: scala.xml.Node, graphKeys: scala.collection.Map[String, GraphMLKey]): Validated[NonEmptyList[Throwable], PartialParsedGraph[N]] = {
    nodeDataType.deserialize(Seq(nodeXml), graphKeys) match {
      case Valid(node) => mergePartialGraphs(nodeXml.child.map {
        case child if child.label == "graph" => parseGraphNodes(child, graphKeys)
        case _ => valid(PartialParsedGraph(None, Map(node.id -> node), Seq.empty))
      })

      case Invalid(error) => invalid(error)
    }
  }

  protected def parseGraphNodes(graphNode: scala.xml.Node, graphKeys: scala.collection.Map[String, GraphMLKey]): ValidatedNel[Throwable, PartialParsedGraph[N]] = {
    val id = GraphMLDatatype.singleAttributeValue("id", graphNode)

    val partialGraphs: Seq[Validated[NonEmptyList[Throwable], PartialParsedGraph[N]]] = graphNode.child.zipWithIndex.map {
      case (nodeXml: scala.xml.Node, _: Int) if nodeXml.label == "node" =>
        parseNode(nodeXml, graphKeys: scala.collection.Map[String, GraphMLKey])
      case (edge: scala.xml.Node, _: Int) if edge.label == "edge" =>
        valid(PartialParsedGraph[N](None, Map.empty, Seq(edge)))

      case _ => valid(PartialParsedGraph.empty[N])
    }

    mergePartialGraphs(partialGraphs).map(_.copy(id = id))
  }

}

object GraphMLDatatype {

  def apply[V, N, M](implicit graphMLDatatype: Datatype[Graph[GraphMLEdge[V], GraphMLNode[N], GraphMLGraph[M]]]) = graphMLDatatype

  protected[graphml] def singleAttributeValue(attributeName: String, node: scala.xml.Node): Option[String] = {
    node.attribute(attributeName).getOrElse(Seq.empty).headOption.map(_.text)
  }

  protected[graphml] def parseProperties(node: Node): Seq[GraphMLProperty] = {
    node.child.flatMap {
      case data if data.label == "data" =>
        GraphMLDatatype.singleAttributeValue("key", data).map { keyId =>
          val value = if (data.child.exists(!_.isInstanceOf[Text])) data.child else data.child.text
          GraphMLProperty(keyId, value)
        }
      case _ => None
    }
  }

  protected[graphml] def parseKeys(rootElem: scala.xml.Node): scala.collection.Map[String, GraphMLKey] =
    rootElem.child.filter(_.label.toLowerCase == "key").flatMap { keyElem =>
      GraphMLDatatype.singleAttributeValue("id", keyElem).map(id => {
        (id, GraphMLKey(
          id = id,
          name = GraphMLDatatype.singleAttributeValue("attr.name", keyElem),
          targetHint = GraphMLDatatype.singleAttributeValue("for", keyElem),
          typeHint = GraphMLDatatype.singleAttributeValue("attr.type", keyElem),
          graphsType = GraphMLDatatype.singleAttributeValue("graphs.type", keyElem),
          yfilesType = GraphMLDatatype.singleAttributeValue("yfiles.type", keyElem)))
      })
    }.toMap
}
