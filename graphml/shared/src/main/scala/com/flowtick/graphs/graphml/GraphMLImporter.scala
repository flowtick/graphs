package com.flowtick.graphs.graphml

import scala.collection.mutable
import scala.util._
import scala.xml.{ Node, Text, XML }

class GraphMLImporter {
  def fromXml(graphml: String): Either[Throwable, GraphMLGraph] =
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

  protected def extractEdgeLabel(properties: Map[String, GraphMLProperty]): Option[String] = {
    properties.values.find(_.key.yfilesType.exists(_ == "edgegraphics")).flatMap { edgeGraphics =>
      edgeGraphics.value match {
        case xml: Seq[scala.xml.Node] =>
          val edgeLabel: Option[Node] = xml.foldLeft(Seq.empty[scala.xml.Node])((a, b) => a ++ b.nonEmptyChildren).find(_.label == "EdgeLabel")
          edgeLabel.map(_.text.trim)
      }
    }
  }

  protected def parseGraphNode(graphNode: scala.xml.Node, keys: Seq[GraphMLKey]): GraphMLGraph = {
    GraphMLGraph.create(singleAttributeValue("id", graphNode).getOrElse("graph")) { implicit g =>
      val nodes = new mutable.HashMap[String, GraphMLNode]()
      val edgeXmlNodes = new mutable.ListBuffer[scala.xml.Node]()

      graphNode.child.zipWithIndex.foreach {
        case (node: scala.xml.Node, index: Int) if node.label == "node" =>
          val id = singleAttributeValue("id", node).getOrElse(node.label)
          val graphNode = GraphMLNode(id, None, parseProperties(node, keys))
          nodes.put(id, graphNode)
          g.addNode(graphNode)
        case (edge: scala.xml.Node, index: Int) if edge.label == "edge" =>
          edgeXmlNodes.append(edge)
        case _ =>
      }

      edgeXmlNodes.foreach { edgeNode =>
        val edgeId = singleAttributeValue("id", edgeNode).getOrElse("edge")

        for {
          source <- singleAttributeValue("source", edgeNode)
          target <- singleAttributeValue("target", edgeNode)
          sourceNode <- nodes.get(source)
          targetNode <- nodes.get(target)
        } yield {
          val properties = parseProperties(edgeNode, keys)
          val graphEdge = GraphMLEdge(
            edgeId,
            extractEdgeLabel(properties),
            sourceNode,
            targetNode,
            properties)
          g.addEdge(graphEdge)
        }
      }
    }
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