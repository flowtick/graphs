package com.flowtick.graphs.graphml

import cats.data.Validated._
import cats.data.ValidatedNel
import com.flowtick.graphs.{ Edge, Graph, Identifiable }
import xmls.XMLS

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util._
import scala.xml.{ Node, NodeSeq, Text }

object GraphMLImporter {

  implicit def graphDeserializer[V, N, M](implicit
    edgeDeserializer: Deserializer[GraphMLEdge[V]],
    nodeDeserializer: Deserializer[GraphMLNode[N]]) = new Deserializer[Graph[GraphMLEdge[V], GraphMLNode[N], GraphMLGraph[M]]] {
    override def deserialize(from: NodeSeq): ValidatedNel[Throwable, Graph[GraphMLEdge[V], GraphMLNode[N], GraphMLGraph[M]]] = {
      def parseGraphNode(graphNode: scala.xml.Node, keys: Seq[GraphMLKey]): Graph[GraphMLEdge[V], GraphMLNode[N], GraphMLGraph[M]] = {
        val edgeXmlNodes = new mutable.ListBuffer[scala.xml.Node]()
        val nodes: mutable.Map[String, GraphMLNode[N]] = parseGraphNodes[N](graphNode, keys, edgeXmlNodes)

        val edges = edgeXmlNodes.flatMap { edgeNode =>
          val edgeId = singleAttributeValue("id", edgeNode).getOrElse("edge")

          for {
            source <- singleAttributeValue("source", edgeNode)
            target <- singleAttributeValue("target", edgeNode)
            sourceNode <- nodes.get(source)
            targetNode <- nodes.get(target)
          } yield {
            val properties = parseProperties(edgeNode, keys)

            edgeDeserializer.deserialize(edgeNode.nonEmptyChildren).map { value =>
              val mlEdge = GraphMLEdge(
                edgeId,
                value,
                extractEdgeLabel(properties),
                properties)

              Edge[GraphMLEdge[V], GraphMLNode[N]](mlEdge.value, sourceNode, targetNode)
            }.toOption
          }
        }.flatten

        Graph.create(GraphMLGraph[M](???, singleAttributeValue("id", graphNode)), nodes.values, edges)
      }

      from.headOption match {
        case Some(root) =>
          root.child.find(_.label.toLowerCase == "graph") match {
            case Some(graph) => validNel(parseGraphNode(graph, parseKeys(root)))
            case None => invalidNel(new IllegalArgumentException("graph node not found"))
          }

        case None => invalidNel(new IllegalArgumentException("cant create graph from empty xml"))
      }
    }
  }

  def fromXml[V, N, M](graphml: String)(implicit
    identifiable: Identifiable[GraphMLNode[N]],
    deserializer: Deserializer[Graph[GraphMLEdge[V], GraphMLNode[N], GraphMLGraph[M]]]): Either[Throwable, Graph[GraphMLEdge[V], GraphMLNode[N], GraphMLGraph[M]]] = {
    XMLS.parse(graphml) match {
      case Right(rootElem) if rootElem.label.toLowerCase == "graphml" => deserializer.deserialize(Seq(rootElem)).toEither.left.map(_.head)
      case Right(nonGraphMl) => Left(new IllegalArgumentException(s"parsed elem is not a graphml element: $nonGraphMl"))
      case Left(error) => Left(error)
    }
  }

  protected def singleAttributeValue(attributeName: String, node: scala.xml.Node): Option[String] = {
    node.attribute(attributeName).getOrElse(Seq.empty).headOption.map(_.text)
  }

  protected def parseKeys(rootElem: scala.xml.Node): Seq[GraphMLKey] = {
    rootElem.child.filter(_.label.toLowerCase == "key").flatMap { keyElem =>
      singleAttributeValue("id", keyElem).map(id => {
        GraphMLKey(
          id = id,
          name = singleAttributeValue("attr.name", keyElem),
          targetHint = singleAttributeValue("for", keyElem),
          typeHint = singleAttributeValue("attr.type", keyElem),
          yfilesType = singleAttributeValue("yfiles.type", keyElem))
      })
    }
  }

  protected def extractNodeLabel(properties: Seq[GraphMLProperty]): Option[String] = {
    properties.find(_.key.yfilesType.exists(_ == "nodegraphics")).flatMap { nodeGraphics =>
      nodeGraphics.value match {
        case xml: Seq[scala.xml.Node @unchecked] =>
          val nodeLabel: Option[Node] = xml.foldLeft(Seq.empty[scala.xml.Node])((a, b) => a ++ b.nonEmptyChildren).find(_.label == "NodeLabel")
          nodeLabel.map(_.text.trim)
      }
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

  protected def parseGraphNodes[N](
    graphNode: scala.xml.Node,
    keys: Seq[GraphMLKey],
    edgeXmlNodes: mutable.ListBuffer[scala.xml.Node])(implicit nodeDeserializer: Deserializer[GraphMLNode[N]]): mutable.Map[String, GraphMLNode[N]] = {
    val nodes = new mutable.HashMap[String, GraphMLNode[N]]()

    graphNode.child.zipWithIndex.foreach {
      case (node: scala.xml.Node, _: Int) if node.label == "node" =>
        val id = singleAttributeValue("id", node).getOrElse(node.label)
        val nodeProperties = parseProperties(node, keys)

        nodeDeserializer.deserialize(node.nonEmptyChildren).map { value =>
          val graphNode = GraphMLNode[N](id, value.value, extractNodeLabel(nodeProperties), nodeProperties)
          nodes.put(id, graphNode)

          node.child.foreach {
            case child if child.label == "graph" => nodes ++= parseGraphNodes(child, keys, edgeXmlNodes)(nodeDeserializer)
            case _ =>
          }
        }

      case (edge: scala.xml.Node, _: Int) if edge.label == "edge" =>
        edgeXmlNodes.append(edge)
      case _ =>
    }
    nodes
  }

  private def parseProperties(node: Node, keys: Seq[GraphMLKey]): Seq[GraphMLProperty] = {
    val properties = new ListBuffer[GraphMLProperty]()
    node.child.foreach {
      case data if data.label == "data" =>
        singleAttributeValue("key", data).foreach { keyId =>
          val value = if (data.child.exists(!_.isInstanceOf[Text])) data.child else data.child.text
          keys.find(_.id == keyId).foreach(key => properties += GraphMLProperty(key, value))
        }
      case _ =>
    }
    properties
  }
}