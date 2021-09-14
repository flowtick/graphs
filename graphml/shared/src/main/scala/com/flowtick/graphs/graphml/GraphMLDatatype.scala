package com.flowtick.graphs.graphml

import cats._
import cats.data.Validated._
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.implicits._
import com.flowtick.graphs.{Edge, Graph, Identifiable, Labeled, Node}

import scala.collection.mutable
import scala.xml.{NodeSeq, Text, Node => XmlNode}

private[graphml] case class PartialParsedGraph[N](
    id: Option[String],
    nodes: scala.collection.Map[String, Node[GraphMLNode[N]]],
    edgesXml: List[scala.xml.Node]
)

private[graphml] object PartialParsedGraph {
  def empty[N]: PartialParsedGraph[N] =
    PartialParsedGraph[N](None, scala.collection.mutable.Map.empty, List.empty)

  implicit def partialGraphSemiGroup[N]: Semigroup[PartialParsedGraph[N]] =
    new Semigroup[PartialParsedGraph[N]] {
      override def combine(
          x: PartialParsedGraph[N],
          y: PartialParsedGraph[N]
      ): PartialParsedGraph[N] =
        PartialParsedGraph(None, x.nodes ++ y.nodes, x.edgesXml ++ y.edgesXml)
    }
}

final case class GraphMLProperty(
    key: String,
    xml: XmlNode,
    typeHint: Option[String] = None
) {
  def nodeValue: Object =
    if (xml.child.exists(!_.isInstanceOf[Text])) xml.child else xml.child.text
}

class GraphMLDatatype[E, N](
    nodeDataType: Datatype[GraphMLNode[N]],
    edgeDataType: Datatype[GraphMLEdge[E]]
)(implicit edgeLabel: Labeled[Edge[GraphMLEdge[E]], String])
    extends Datatype[GraphMLGraph[E, N]] {
  val nodeTargetHint = Some("node")
  val edgeTargetHint = Some("edge")
  val metaTargetHint = Some("meta")

  override def keys(targetHint: Option[String]): Seq[GraphMLKey] = Seq.empty

  override def serialize(
      g: GraphMLGraph[E, N],
      targetHint: Option[String]
  ): NodeSeq = {

    def graphKeys: Iterable[XmlNode] = (nodeDataType.keys(
      nodeTargetHint
    ) ++ edgeDataType.keys(edgeTargetHint) ++ g.meta.keys).map { key: GraphMLKey =>
      // format: OFF
      <key id={ key.id }
           attr.name={ key.name.getOrElse(key.id) }
           for={ key.targetHint.map(Text(_)) }
           yfiles.type={ key.yfilesType.map(Text(_)) }
           attr.type={ key.typeHint.map(Text(_)) }
           graphs.type={key.graphsType.map(Text(_))} />
      // format: ON
    }

    def edgesXml: Iterator[XmlNode] =
      g.graph.edges.iterator.flatMap(edge => edgeDataType.serialize(edge.value, edgeTargetHint))
    def nodesXml: Iterator[XmlNode] =
      g.graph.nodes.iterator.flatMap(node => nodeDataType.serialize(node.value, nodeTargetHint))

    // format: OFF
    <graphml xmlns="http://graphml.graphdrawing.org/xmlns" xmlns:java="http://www.yworks.com/xml/yfiles-common/1.0/java" xmlns:sys="http://www.yworks.com/xml/yfiles-common/markup/primitives/2.0" xmlns:x="http://www.yworks.com/xml/yfiles-common/markup/2.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:y="http://www.yworks.com/xml/graphml" xmlns:yed="http://www.yworks.com/xml/yed/3" xsi:schemaLocation="http://graphml.graphdrawing.org/xmlns http://www.yworks.com/xml/schema/graphml/1.1/ygraphml.xsd">
      <!-- Created by https://github.com/flowtick/graphs GraphML renderer -->
      { graphKeys }
      <graph id="G" edgedefault="directed">
        { nodesXml }
        { edgesXml }
      </graph>
    </graphml>
    // format: ON
  }

  override def deserialize(
      from: NodeSeq,
      graphKeys: scala.collection.Map[String, GraphMLKey],
      targetHint: Option[String]
  ): ValidatedNel[Throwable, GraphMLGraph[E, N]] =
    from.headOption match {
      case Some(root) =>
        val resources: Seq[GraphMLResource] = GraphMLDatatype
          .parseProperties(root)
          .filter(property =>
            graphKeys
              .get(property.key)
              .exists(key =>
                key.yfilesType.contains("resources") && key.targetHint
                  .contains("graphml")
              )
          )
          .flatMap(property =>
            property.xml.nonEmptyChildren.headOption
              .filter(_.label == "Resources")
              .map(GraphMLDatatype.parseResources)
              .getOrElse(List.empty)
          )
          .toList

        root.child.iterator
          .find(_.label == "graph")
          .map(parseGraphRoot(_, graphKeys, resources))
          .getOrElse(
            invalidNel(
              new IllegalArgumentException(
                s"unable to find graph node in ${root.toString}"
              )
            )
          )

      case None =>
        invalidNel(new IllegalArgumentException("cant parse empty xml"))
    }

  protected def parseGraphRoot(
      graph: XmlNode,
      graphKeys: scala.collection.Map[String, GraphMLKey],
      resources: Seq[GraphMLResource]
  ): Validated[NonEmptyList[Throwable], GraphMLGraph[E, N]] =
    parseGraphNodes(graph, graphKeys).andThen { parsedGraph =>
      parseEdges(parsedGraph.edgesXml, parsedGraph.nodes, graphKeys).andThen { edges =>
        valid(
          GraphMLGraph(
            Graph(edges, parsedGraph.nodes.values),
            GraphMLMeta(parsedGraph.id, graphKeys.values.toSeq, resources)
          )
        )
      }
    }

  protected def parseEdges(
      edgeXmlNodes: List[scala.xml.Node],
      nodes: scala.collection.Map[String, Node[GraphMLNode[N]]],
      keys: scala.collection.Map[String, GraphMLKey]
  ): Validated[NonEmptyList[Throwable], List[Edge[GraphMLEdge[E]]]] = {
    edgeXmlNodes.map { edgeNode =>
      val edge =
        edgeDataType.deserialize(Seq(edgeNode), keys, edgeTargetHint).andThen { mlEdge =>
          (for {
            source <- GraphMLDatatype.singleAttributeValue("source", edgeNode)
            target <- GraphMLDatatype.singleAttributeValue("target", edgeNode)
            id <- GraphMLDatatype
              .singleAttributeValue("id", edgeNode)
              .orElse(Some(s"$source-$target"))
          } yield {
            validNel(Edge[GraphMLEdge[E]](id, mlEdge, source, target))
          }).getOrElse(
            invalidNel(
              new IllegalArgumentException(
                s"unable to parse edge from ${edgeNode.toString}"
              )
            )
          )
        }
      edge
    }
  }.sequence

  protected def mergePartialGraphs(
      graphs: Iterator[ValidatedNel[Throwable, PartialParsedGraph[N]]]
  ): ValidatedNel[Throwable, PartialParsedGraph[N]] =
    graphs.foldLeft(
      validNel[Throwable, PartialParsedGraph[N]](PartialParsedGraph.empty[N])
    )(_ combine _)

  protected def parseNode(
      nodeXml: scala.xml.Node,
      graphKeys: scala.collection.Map[String, GraphMLKey]
  ): Validated[NonEmptyList[Throwable], PartialParsedGraph[N]] = {
    nodeDataType.deserialize(Seq(nodeXml), graphKeys, nodeTargetHint) match {
      case Valid(node) =>
        mergePartialGraphs(nodeXml.child.iterator.map {
          case child if child.label == "graph" =>
            parseGraphNodes(child, graphKeys)
          case _ =>
            valid(
              PartialParsedGraph(
                None,
                Map(node.id -> Node.of(node)),
                List.empty
              )
            )
        })

      case Invalid(error) => invalid(error)
    }
  }

  protected def parseGraphNodes(
      graphNode: scala.xml.Node,
      graphKeys: scala.collection.Map[String, GraphMLKey]
  ): ValidatedNel[Throwable, PartialParsedGraph[N]] = {
    val id = GraphMLDatatype.singleAttributeValue("id", graphNode)

    val partialGraphs: Iterator[Validated[NonEmptyList[Throwable], PartialParsedGraph[N]]] =
      graphNode.child.iterator.zipWithIndex.map {
        case (nodeXml: scala.xml.Node, _: Int) if nodeXml.label == "node" =>
          parseNode(
            nodeXml,
            graphKeys: scala.collection.Map[String, GraphMLKey]
          )

        case (edge: scala.xml.Node, _: Int) if edge.label == "edge" =>
          valid(PartialParsedGraph[N](None, mutable.TreeMap.empty, List(edge)))

        case _ => valid(PartialParsedGraph.empty[N])
      }

    mergePartialGraphs(partialGraphs).map(_.copy(id = id))
  }
}

object GraphMLDatatype {
  def apply[E, N](implicit
      identifiable: Identifiable[GraphMLNode[N]],
      edgeLabel: Labeled[Edge[GraphMLEdge[E]], String],
      nodeDataType: Datatype[N],
      edgeDataType: Datatype[E]
  ): Datatype[GraphMLGraph[E, N]] =
    new GraphMLDatatype[E, N](
      new GraphMLNodeDatatype[N](nodeDataType),
      new GraphMLEdgeDatatype[E](edgeDataType)
    )

  protected[graphml] def singleAttributeValue(
      attributeName: String,
      node: scala.xml.Node
  ): Option[String] = {
    node.attribute(attributeName).map(_.text)
  }

  protected[graphml] def parseProperties(
      node: XmlNode
  ): Iterator[GraphMLProperty] =
    node.child.iterator.flatMap {
      case data if data.label == "data" =>
        val typeHint =
          GraphMLDatatype.singleAttributeValue("type", data).filter(_.nonEmpty)
        GraphMLDatatype.singleAttributeValue("key", data).map { keyId =>
          GraphMLProperty(keyId, data, typeHint)
        }
      case _ => None
    }

  protected[graphml] def parseResources(
      resourcesNode: XmlNode
  ): Iterator[GraphMLResource] =
    resourcesNode.child.iterator.flatMap {
      case data if data.label == "Resource" =>
        for {
          id <- GraphMLDatatype
            .singleAttributeValue("id", data)
            .filter(_.nonEmpty)
        } yield GraphMLResource(
          id,
          data.text,
          GraphMLDatatype
            .singleAttributeValue("type", data)
            .filter(_.nonEmpty)
            .orElse(Some("svg"))
        )
      case _ => None
    }

  protected[graphml] def parseKeys(
      rootElem: scala.xml.Node
  ): scala.collection.Map[String, GraphMLKey] =
    rootElem.child.iterator
      .filter(_.label.toLowerCase == "key")
      .flatMap { keyElem =>
        GraphMLDatatype
          .singleAttributeValue("id", keyElem)
          .map(id => {
            (
              id,
              GraphMLKey(
                id = id,
                name = GraphMLDatatype.singleAttributeValue("attr.name", keyElem),
                targetHint = GraphMLDatatype.singleAttributeValue("for", keyElem),
                typeHint = GraphMLDatatype.singleAttributeValue("attr.type", keyElem),
                graphsType = GraphMLDatatype.singleAttributeValue("graphs.type", keyElem),
                yfilesType = GraphMLDatatype.singleAttributeValue("yfiles.type", keyElem)
              )
            )
          })
      }
      .toMap

  protected[graphml] def isValueProperty(
      property: GraphMLProperty,
      graphKeys: collection.Map[String, GraphMLKey],
      valueKeys: Seq[String]
  ): Boolean = {
    graphKeys
      .get(property.key)
      .exists(key =>
        key.graphsType.isDefined || key.name.exists(keyName => valueKeys.contains(keyName))
      )
  }

}
