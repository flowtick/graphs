package com.flowtick.graphs.graphml

import com.flowtick.graphs.{ Edge, Graph, Identifiable }
import com.flowtick.graphs.defaults._
import xmls.XMLS

import scala.collection.mutable
import scala.util._
import scala.xml.{ Node, Text }

object GraphMLImporter {
  def fromXml[V, N](graphml: String)(implicit identifiable: Identifiable[GraphMLNode[N]]): Either[Throwable, Graph[GraphMLEdge[V], GraphMLNode[N], GraphMLGraph]] = {
    def parseGraphNode(graphNode: scala.xml.Node, keys: Seq[GraphMLKey]): Graph[GraphMLEdge[V], GraphMLNode[N], GraphMLGraph] = {
      val edgeXmlNodes = new mutable.ListBuffer[scala.xml.Node]()
      val nodes: mutable.Map[String, GraphMLNode[N]] = parseGraphNodes(graphNode, keys, edgeXmlNodes)

      val edges: mutable.Seq[Edge[GraphMLEdge[V], GraphMLNode[N]]] = edgeXmlNodes.flatMap { edgeNode =>
        val edgeId = singleAttributeValue("id", edgeNode).getOrElse("edge")

        for {
          source <- singleAttributeValue("source", edgeNode)
          target <- singleAttributeValue("target", edgeNode)
          sourceNode <- nodes.get(source)
          targetNode <- nodes.get(target)
        } yield {
          val properties = parseProperties(edgeNode, keys)
          Edge[GraphMLEdge[V], GraphMLNode[N]](GraphMLEdge(
            edgeId,
            ???,
            extractEdgeLabel(properties),
            properties), sourceNode, targetNode)
        }
      }

      Graph.create(GraphMLGraph(id = singleAttributeValue("id", graphNode)), nodes.values, edges)
    }

    XMLS.parse(graphml) match {
      case Right(rootElem) if rootElem.label.toLowerCase == "graphml" =>
        rootElem.child.find(_.label.toLowerCase == "graph") match {
          case Some(graph) => Right(parseGraphNode(graph, parseKeys(rootElem)))
          case None => Left(new IllegalArgumentException("graph node not found"))
        }
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

  protected def parseGraphNodes[N](
    graphNode: scala.xml.Node,
    keys: Seq[GraphMLKey],
    edgeXmlNodes: mutable.ListBuffer[scala.xml.Node]): mutable.HashMap[String, GraphMLNode[N]] = {
    val nodes = new mutable.HashMap[String, GraphMLNode[N]]()

    graphNode.child.zipWithIndex.foreach {
      case (node: scala.xml.Node, _: Int) if node.label == "node" =>
        val id = singleAttributeValue("id", node).getOrElse(node.label)
        val nodeProperties = parseProperties(node, keys)
        val graphNode = GraphMLNode[N](id, ???, extractNodeLabel(nodeProperties), nodeProperties)
        nodes.put(id, graphNode)

        node.child.foreach {
          case child if child.label == "graph" => nodes ++= parseGraphNodes(child, keys, edgeXmlNodes)
          case _ =>
        }

      case (edge: scala.xml.Node, _: Int) if edge.label == "edge" =>
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