package com.flowtick.graphs.layout
import com.flowtick.graphs.{Edge, Graph, Labeled}
import org.eclipse.elk.alg.layered.ElkLayered
import org.eclipse.elk.alg.layered.graph.{LEdge, LGraph, LNode, LPort}
import org.eclipse.elk.alg.layered.options.LayeredOptions
import org.eclipse.elk.alg.mrtree.MrTree
import org.eclipse.elk.alg.mrtree.graph.{TEdge, TGraph, TNode}
import org.eclipse.elk.alg.mrtree.options.MrTreeOptions
import org.eclipse.elk.core.math.{ElkMargin, ElkPadding, KVector, KVectorChain}
import org.eclipse.elk.core.options.{CoreOptions, Direction, PortSide}
import org.eclipse.elk.core.util.{IElkProgressMonitor, NullElkProgressMonitor}
import org.eclipse.elk.graph.util.ElkReflect

import java.util
import scala.concurrent.Future
import scala.reflect.ClassTag

object ELkLayoutJVM extends GraphLayoutOps {
  trait ElkLayout[E, N] {
    def doLayout(graph: Graph[E, N],
                 layoutConfiguration: GraphLayoutConfiguration,
                 monitor: Option[IElkProgressMonitor]): GraphLayoutLike
  }

  class ElkLayeredLayout[E, N] extends ElkLayout[E, N] {
    override def doLayout(g: Graph[E, N], layoutConfiguration: GraphLayoutConfiguration, monitor: Option[IElkProgressMonitor]): GraphLayoutLike = {
      val lGraph = new LGraph

      val nodeMap: Map[String, LNode] = g.nodes.map(node => {
        val lNode = new LNode(lGraph)
        lNode.getSize.set(layoutConfiguration.nodeWidth, layoutConfiguration.nodeHeight)

        val westPort = new LPort()
        westPort.setNode(lNode)
        westPort.setSide(PortSide.WEST)

        val eastPort = new LPort()
        eastPort.setNode(lNode)
        eastPort.setSide(PortSide.EAST)

        lGraph.getLayerlessNodes.add(lNode)

        (node.id, lNode)
      }).toMap

      val edgeMap: Map[String, LEdge] = g.edges.flatMap { edge =>
        for {
          from <- nodeMap.get(edge.from)
          to <- nodeMap.get(edge.to)
        } yield {
          val lEdge = new LEdge
          lEdge.setSource(from.getPorts.get(1))
          lEdge.setTarget(to.getPorts.get(0))

          (edge.id, lEdge)
        }
      }.toMap

      layoutConfiguration.spacingNodeNode.foreach(value => lGraph.setProperty(LayeredOptions.SPACING_NODE_NODE, java.lang.Double.valueOf(value)))
      layoutConfiguration.spacing.foreach(value => lGraph.setProperty(LayeredOptions.SPACING_BASE_VALUE, java.lang.Double.valueOf(value)))
      layoutConfiguration.direction.foreach {
        case LayoutDirection.Up => lGraph.setProperty(CoreOptions.DIRECTION, Direction.UP)
        case LayoutDirection.Down => lGraph.setProperty(CoreOptions.DIRECTION, Direction.DOWN)
        case LayoutDirection.Left => lGraph.setProperty(CoreOptions.DIRECTION, Direction.LEFT)
        case LayoutDirection.Right => lGraph.setProperty(CoreOptions.DIRECTION, Direction.RIGHT)
      }

      val layered = new ElkLayered()
      layered.doLayout(lGraph, monitor.getOrElse(new NullElkProgressMonitor))

      GraphLayout(nodes = nodeMap.map {
        case (id, node) => (id, DefaultGeometry(node.getPosition.x, node.getPosition.y, node.getSize.x, node.getSize.y))
      }, edges = edgeMap.map {
        case (id, edge) => (id, EdgePath(
          edge.getSource.getAnchor.x,
          edge.getSource.getAnchor.y,
          edge.getTarget.getAnchor.x,
          edge.getTarget.getAnchor.y,
          points = edge.getBendPoints.toArray.map { bendPoint =>
            PointSpec(bendPoint.x, bendPoint.y)
          }.toList
        ))
      }, width = Some(lGraph.getActualSize.x), height = Some(lGraph.getActualSize.y))
    }
  }

  case class TreeNode(index: Int, tNode: TNode, nodeId: String)

  class ElkTreeLayout[E, N] extends ElkLayout[E, N] {
    def doLayout(graph: Graph[E, N],
                 layoutConfiguration: GraphLayoutConfiguration,
                 monitor: Option[IElkProgressMonitor]): GraphLayoutLike = {
      val treeLayout = new MrTree

      val tGraph = new TGraph

      layoutConfiguration.spacingNodeNode.foreach(value => tGraph.setProperty(MrTreeOptions.SPACING_NODE_NODE, java.lang.Double.valueOf(value)))
      layoutConfiguration.direction.foreach {
        case LayoutDirection.Up => tGraph.setProperty(CoreOptions.DIRECTION, Direction.UP)
        case LayoutDirection.Down => tGraph.setProperty(CoreOptions.DIRECTION, Direction.DOWN)
        case LayoutDirection.Left => tGraph.setProperty(CoreOptions.DIRECTION, Direction.LEFT)
        case LayoutDirection.Right => tGraph.setProperty(CoreOptions.DIRECTION, Direction.RIGHT)
      }

      val nodeMap: Map[String, TreeNode] = graph.nodes.zipWithIndex.map {
        case (node, index) =>
        val tNode = new TNode(index, tGraph)
        tNode.getSize.set(layoutConfiguration.nodeWidth, layoutConfiguration.nodeHeight)
        tGraph.getNodes.add(tNode)
        (node.id, TreeNode(index, tNode, node.id))
      }.toMap

      val edgeMap: Map[String, TEdge] = graph.edges.zipWithIndex.flatMap {
        case (edge, index) =>
          for {
            source <- nodeMap.get(edge.from)
            target <- nodeMap.get(edge.to)
            tEdge = new TEdge(null, null)
            _ = {
              tEdge.id = index
              tEdge.setSource(source.tNode)
              tEdge.setTarget(target.tNode)
              tGraph.getEdges.add(tEdge)
            }
          } yield (edge.id, tEdge)
      }.toMap

      treeLayout.doLayout(tGraph, monitor.getOrElse(new NullElkProgressMonitor))

      var maxX: Double = 0.0
      var maxY: Double = 0.0

      GraphLayout(nodes = nodeMap.map {
        case (_, node) =>
          maxX = Math.max(node.tNode.getPosition.x, maxX)
          maxY = Math.max(node.tNode.getPosition.y, maxY)

          (node.nodeId, DefaultGeometry(node.tNode.getPosition.x, node.tNode.getPosition.y, node.tNode.getSize.x, node.tNode.getSize.y))
      }, edges = edgeMap.map {
        case (id, edge) => (id, EdgePath(
          points = edge.getBendPoints.toArray.map { bendPoint =>
            PointSpec(bendPoint.x, bendPoint.y)
          }.toList
        ))
      }, width = Some(maxX + layoutConfiguration.nodeWidth), height = Some(maxY + layoutConfiguration.nodeHeight))
    }
  }

  /**
   * without those clone functions, ELK will complain during the layout process
   *
   * @param cloneF
   * @param classTag
   * @tparam T
   */
  private def registerClone[T](cloneF: T => AnyRef)(implicit classTag: ClassTag[T]): Unit = ElkReflect.registerClone(classTag.runtimeClass, new ElkReflect.CloneFunction {
    override def clone(o: Any): AnyRef = classTag.unapply(o).map(cloneF).getOrElse(throw new IllegalArgumentException("wrong type"))
  })

  registerClone[ElkPadding](_.clone)
  registerClone[ElkMargin](_.clone)
  registerClone[util.ArrayList[_]](_.clone)
  registerClone[KVectorChain](_.clone)
  registerClone[KVector](_.clone)

  def getElkLayoutFromType[E, N](layoutType: Option[LayoutType]): ElkLayout[E, N] = {
    layoutType match {
      case Some(LayoutType.Tree) => new ElkTreeLayout
      case _ => new ElkLayeredLayout
    }
  }

  override def layout[E, N](g: Graph[E, N], layoutConfiguration: GraphLayoutConfiguration)(implicit edgeLabel: Labeled[Edge[E], String]): Future[GraphLayoutLike] =
    Future.successful(getElkLayoutFromType(layoutConfiguration.layoutType).doLayout(g, layoutConfiguration, None))
}
