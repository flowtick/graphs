package com.flowtick.graphs.graphml

import com.flowtick.graphs.{ Edge, Graph }

import scala.collection.mutable
import scala.util._
import scala.xml.{ Node, Text, XML }

class GraphMLImporter {
  def fromXml(graphml: String): Either[Throwable, GraphMLGraph[GraphMLNode, GraphMLEdge[GraphMLNode]]] =
    Try(XML.load(new java.io.ByteArrayInputStream(graphml.getBytes)))
      .filter(_.label.toLowerCase == "graphml").flatMap { rootElem =>
        rootElem.child.find(_.label.toLowerCase == "graph") match {
          case Some(graph) => Success(parseGraphNode(graph, parseKeys(rootElem)))
          case None => Failure(new IllegalArgumentException("graph node not found"))
        }
      } match {
        case Success(graph) => Right(graph)
        case Failure(error) => Left(error)
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

  protected def extractNodeLabel(properties: Map[String, GraphMLProperty]): Option[String] = {
    properties.values.find(_.key.yfilesType.exists(_ == "nodegraphics")).flatMap { nodeGraphics =>
      nodeGraphics.value match {
        case xml: Seq[scala.xml.Node @unchecked] =>
          val nodeLabel: Option[Node] = xml.foldLeft(Seq.empty[scala.xml.Node])((a, b) => a ++ b.nonEmptyChildren).find(_.label == "NodeLabel")
          nodeLabel.map(_.text.trim)
      }
    }
  }

  protected def extractEdgeLabel(properties: Map[String, GraphMLProperty]): Option[String] = {
    properties.values.find(_.key.yfilesType.exists(_ == "edgegraphics")).flatMap { edgeGraphics =>
      edgeGraphics.value match {
        case xml: Seq[scala.xml.Node @unchecked] =>
          val edgeLabel: Option[Node] = xml.foldLeft(Seq.empty[scala.xml.Node])((a, b) => a ++ b.nonEmptyChildren).find(_.label == "EdgeLabel")
          edgeLabel.map(_.text.trim)
      }
    }
  }

  protected def parseGraphNode(graphNode: scala.xml.Node, keys: Seq[GraphMLKey]): GraphMLGraph[GraphMLNode, GraphMLEdge[GraphMLNode]] = {
    val edgeXmlNodes = new mutable.ListBuffer[scala.xml.Node]()
    val nodes: mutable.Map[String, GraphMLNode] = parseGraphNodes(graphNode, keys, edgeXmlNodes)

    val edges: mutable.Seq[Edge[GraphMLEdge[GraphMLNode], GraphMLNode]] = edgeXmlNodes.flatMap { edgeNode =>
      val edgeId = singleAttributeValue("id", edgeNode).getOrElse("edge")

      for {
        source <- singleAttributeValue("source", edgeNode)
        target <- singleAttributeValue("target", edgeNode)
        sourceNode <- nodes.get(source)
        targetNode <- nodes.get(target)
      } yield {
        val properties = parseProperties(edgeNode, keys)
        GraphMLEdge[GraphMLNode](
          edgeId,
          extractEdgeLabel(properties),
          sourceNode,
          Some(targetNode),
          properties)
      }
    }

    GraphMLGraph[GraphMLNode, GraphMLEdge[GraphMLNode]](
      singleAttributeValue("id", graphNode),
      Graph[GraphMLNode, GraphMLEdge[GraphMLNode]](nodes.values.toSet, edges.toSet))
  }

  protected def parseGraphNodes(
    graphNode: scala.xml.Node,
    keys: Seq[GraphMLKey],
    edgeXmlNodes: mutable.ListBuffer[scala.xml.Node]): mutable.HashMap[String, GraphMLNode] = {
    val nodes = new mutable.HashMap[String, GraphMLNode]()

    graphNode.child.zipWithIndex.foreach {
      case (node: scala.xml.Node, index: Int) if node.label == "node" =>
        val id = singleAttributeValue("id", node).getOrElse(node.label)
        val nodeProperties = parseProperties(node, keys)
        val graphNode = GraphMLNode(id, extractNodeLabel(nodeProperties), nodeProperties)
        nodes.put(id, graphNode)

        node.child.foreach {
          case child if child.label == "graph" => nodes ++= parseGraphNodes(child, keys, edgeXmlNodes)
          case _ =>
        }

      case (edge: scala.xml.Node, index: Int) if edge.label == "edge" =>
        edgeXmlNodes.append(edge)
      case _ =>
    }
    nodes
  }

  private def parseProperties(node: Node, keys: Seq[GraphMLKey]): Map[String, GraphMLProperty] = {
    val properties = mutable.HashMap[String, GraphMLProperty]()
    node.child.foreach {
      case data if data.label == "data" =>
        singleAttributeValue("key", data).foreach { keyId =>
          val value = if (data.child.exists(!_.isInstanceOf[Text])) data.child else data.child.text
          keys.find(_.id == keyId).foreach(key => properties.put(key.id, GraphMLProperty(key, value)))
        }
      case _ =>
    }
    properties.toMap
  }
}