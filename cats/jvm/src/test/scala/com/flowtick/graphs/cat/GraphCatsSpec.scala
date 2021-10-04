package com.flowtick.graphs.cat

import cats.Applicative
import com.flowtick.graphs._
import com.flowtick.graphs.defaults._
import cats.implicits._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GraphCatsSpec extends AnyFlatSpec with Matchers {
  import com.flowtick.graphs.cat.instances._

  "Graph Monoid" should "combine graphs" in {
    type NumberNodeGraph = Graph[Unit, Int]
    import com.flowtick.graphs.defaults.id._

    val graphA: NumberNodeGraph =
      Graph.fromEdges(Set(1 --> 2, 2 --> 3)).withNode(Node.of(10))

    val graphB: NumberNodeGraph =
      Graph.fromEdges(Set(2 --> 3, 4 --> 3, 4 --> 5, 5 --> 1))

    val combined = graphA |+| graphB

    combined.edges should contain theSameElementsAs Seq(
      1 --> 2,
      2 --> 3,
      4 --> 3,
      4 --> 5,
      5 --> 1
    ).flatMap(_.toEdges)

    combined.nodes.map(_.value) should contain theSameElementsAs Seq(
      1, 2, 3, 4, 5, 10
    )
  }

  "Graph Applicative" should "map over nodes" in {
    val addOne = (x: Int) => x + 1
    val timesTwo = (x: Int) => x * 2

    implicit val id: Identifiable[Any] = {
      case f if f == addOne   => "addOne"
      case f if f == timesTwo => "timesTwo"
      case other              => other.toString
    }

    val functionGraph: Graph[Unit, Int => Int] = Graph.fromEdges(
      Set(addOne --> timesTwo)
    )

    val numberGraph: Graph[Unit, Int] = Graph.fromEdges[Unit, Int](Set(1 --> 2, 2 --> 3))

    val applied = Applicative[({ type GraphType[T] = Graph[Unit, T] })#GraphType]
      .ap(functionGraph)(numberGraph)

    applied should be(
      Graph.fromEdges[Unit, Int](
        Set(2 --> 3, 3 --> 4) ++ Set(2 --> 4, 4 --> 6)
      )
    )
  }

}
