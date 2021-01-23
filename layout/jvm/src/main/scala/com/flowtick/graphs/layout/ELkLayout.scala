package com.flowtick.graphs.layout
import com.flowtick.graphs.{Edge, Graph, Labeled}
import org.eclipse.elk.alg.layered.ElkLayered
import org.eclipse.elk.alg.layered.graph.{LEdge, LGraph, LNode, LPort}
import org.eclipse.elk.alg.layered.options.LayeredOptions
import org.eclipse.elk.core.math.{ElkMargin, ElkPadding, KVector, KVectorChain}
import org.eclipse.elk.core.options.{Direction, PortSide}
import org.eclipse.elk.core.util.NullElkProgressMonitor
import org.eclipse.elk.graph.util.ElkReflect

import java.util
import scala.reflect.ClassTag

object ELkLayout extends GraphLayoutOps {
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

  override def layout[E, N](g: Graph[E, N], layoutConfiguration: GraphLayoutConfiguration)(implicit edgeLabel: Labeled[Edge[E], String]): GraphLayoutLike = {
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

    val layered = new ElkLayered()

    layoutConfiguration.spacingNodeNode.foreach(value => lGraph.setProperty(LayeredOptions.SPACING_NODE_NODE, java.lang.Double.valueOf(value)))
    layoutConfiguration.spacing.foreach(value => lGraph.setProperty(LayeredOptions.SPACING_BASE_VALUE, java.lang.Double.valueOf(value)))
    layoutConfiguration.direction.foreach {
      case Up => lGraph.setProperty(LayeredOptions.DIRECTION, Direction.UP)
      case Down => lGraph.setProperty(LayeredOptions.DIRECTION, Direction.DOWN)
      case Left => lGraph.setProperty(LayeredOptions.DIRECTION, Direction.LEFT)
      case Right => lGraph.setProperty(LayeredOptions.DIRECTION, Direction.RIGHT)
    }

    layered.doLayout(lGraph, new NullElkProgressMonitor)

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
    })
  }
}
