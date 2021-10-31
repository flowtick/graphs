package com.flowtick.graphs.layout
import com.flowtick.graphs.util.MathUtil.Vector2
import com.flowtick.graphs.{Edge, Graph, Labeled, Node}

import scala.collection.mutable
import scala.concurrent.Future
import scala.util.Random

final case class Point(
    position: Vector2,
    mass: Double,
    velocity: Vector2 = Vector2.zero,
    acceleration: Vector2 = Vector2.zero
) {
  def applyForce(force: Vector2): Point = copy(acceleration = acceleration + (force / mass))
}

final case class Spring(start: Point, end: Point, length: Double, stiffness: Double) {
  def applyForce(amount: Double): Spring = {
    val d = end.position.-(start.position) // the direction of the theSpring
    val displacement = length - d.length
    val direction = d.normalise

    copy(
      start = start.applyForce(direction.times(stiffness * displacement * -amount)),
      end = end.applyForce(direction.times(stiffness * displacement * amount))
    )
  }
}

// adapted from https://github.com/dhotson/springy/blob/master/springy.js
class ForceDirectedLayout private (
    stiffness: Double = 400.0,
    repulsion: Double = 400.0,
    damping: Double = 0.5,
    minEnergyThreshold: Double = 0.01,
    maxSpeed: Double = Double.PositiveInfinity,
    seed: Option[Long] = None,
    maxTicks: Int = 1000
) {
  private val nodePoints: mutable.Map[String, Point] = mutable.Map()
  private val edgeSprings: mutable.Map[String, Spring] = mutable.Map()
  private val random = seed.fold(new Random())(new Random(_))

  private def point(nodeId: String): Point =
    nodePoints.get(nodeId) match {
      case Some(existing) => existing
      case None =>
        val point = Point(position = Vector2.random(random), 1.0)
        nodePoints += nodeId -> point
        point
    }

  private def findOutgoingSpring[E, N](g: Graph[E, N], nodeId: String): Option[Spring] =
    g
      .outgoing(nodeId)
      .map(edge => edgeSprings.get(edge.id))
      .filter(_.isDefined)
      .lastOption
      .flatten

  private def spring[E, N](g: Graph[E, N], edge: Edge[E]): Spring =
    edgeSprings.get(edge.id) match {
      case Some(existing) => existing
      case None =>
        val length = 1.0
        val connectedSpring =
          findOutgoingSpring(g, edge.from).orElse(findOutgoingSpring(g, edge.to))

        connectedSpring.getOrElse {
          val newSpring = Spring(
            point(edge.from),
            point(edge.to),
            length,
            stiffness
          )
          edgeSprings += edge.id -> newSpring
          newSpring
        }
    }

  private def eachNode[E, N, T](g: Graph[E, N])(f: (Node[N], Point) => T): Iterable[T] =
    g.nodes.map { node =>
      f(node, point(node.id))
    }

  private def eachSpring[E, N](g: Graph[E, N])(f: Spring => Spring): Unit =
    g.edges.map { edge =>
      f(spring(g, edge))
    }

  def applyCoulombsLaw[E, N](g: Graph[E, N]): Iterable[Iterable[Any]] =
    eachNode(g) { case (node1, point1) =>
      eachNode(g) { case (node2, point2) =>
        if (point1 != point2) {
          val d = point1.position - point2.position
          val distance =
            d.length + 0.1 // avoid massive forces at small distances (and divide by zero)
          val direction = d.normalise

          // apply force to each end point
          nodePoints += node1.id -> point1.applyForce(
            direction.times(repulsion)./(distance * distance * 0.5)
          )
          nodePoints += node2.id -> point2.applyForce(
            direction.times(repulsion)./(distance * distance * -0.5)
          )
        }
      }
    }

  def applyHookesLaw[E, N](g: Graph[E, N]): Unit = eachSpring(g)(_.applyForce(0.5))

  def attractToCentre[E, N](g: Graph[E, N]): Unit =
    eachNode(g) { case (node, point) =>
      val direction = point.position.times(-1.0)
      nodePoints += node.id -> point.applyForce(direction.times(repulsion / 50.0))
    }

  def updateVelocity[E, N](g: Graph[E, N])(timestep: Double): Unit =
    eachNode(g) { case (node, point) =>
      val velocity = point.velocity.+(point.acceleration.times(timestep)).times(damping)

      val withMaximum = if (velocity.length > maxSpeed) {
        point.velocity.normalise.times(maxSpeed)
      } else velocity

      nodePoints += node.id -> point.copy(velocity = withMaximum, acceleration = Vector2.zero)
    }

  def updatePosition[E, N](g: Graph[E, N])(timestep: Double): Unit =
    eachNode(g) { case (node, point) =>
      nodePoints += node.id -> point.copy(position =
        point.position.+(point.velocity.times(timestep))
      )
    }

  def totalEnergy[E, N](g: Graph[E, N]): Double =
    eachNode(g) { case (_, point) =>
      val speed = point.velocity.length
      0.5 * point.mass * speed * speed
    }.sum

  def tick[E, N](g: Graph[E, N])(timestep: Double): ForceDirectedLayout = {
    applyCoulombsLaw(g)
    applyHookesLaw(g)
    attractToCentre(g)
    updateVelocity(g)(timestep)
    updatePosition(g)(timestep)
    this
  }

  def toLayout[E, N](
      g: Graph[E, N],
      layoutConfiguration: GraphLayoutConfiguration,
      stepSize: Double = 0.03
  ): GraphLayout = {
    val (layout, _) = (1 to maxTicks).view
      .map(step => (tick(g)(stepSize), step))
      .find {
        case (layout, _) if layout.totalEnergy(g) < minEnergyThreshold => true
        case _                                                         => false
      }
      .getOrElse((this, maxTicks))

    val scale = layoutConfiguration.scale.getOrElse(20.0)
    val (minX, minY) = layout.nodePoints.values.foldLeft((0.0, 0.0)) { case ((x, y), point) =>
      (Math.min(x, point.position.x), Math.min(y, point.position.y))
    }

    val nodeGeometry = layout.nodePoints.view.map { case (key, point) =>
      key -> DefaultGeometry(
        Math.round((point.position.x + Math.abs(minX)) * scale),
        Math.round((point.position.y + Math.abs(minY)) * scale),
        layoutConfiguration.nodeWidth,
        layoutConfiguration.nodeHeight
      )
    }.toMap

    GraphLayout(nodeGeometry)
  }

}

object ForceDirectedLayout extends GraphLayoutOps {
  override def layout[E, N](g: Graph[E, N], layoutConfiguration: GraphLayoutConfiguration)(implicit
      edgeLabel: Labeled[Edge[E], String]
  ): Future[GraphLayoutLike] =
    Future.successful(
      new ForceDirectedLayout(seed = layoutConfiguration.seed).toLayout(g, layoutConfiguration)
    )
}
