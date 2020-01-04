trait GraphMLExample {
  {
    // #simple-graphml
    import com.flowtick.graphs.Graph
    import com.flowtick.graphs.defaults._
    import com.flowtick.graphs.graphml._
    import com.flowtick.graphs.graphml.generic._

    val simple: Graph[Unit, String, Unit] = Graph.fromEdges(Set(
      n("A") --> n("B"),
      n("B") --> n("C"),
      n("D") --> n("A")))

    val graphML = simple.asGraphML().xml
    val loaded = FromGraphML[Int, String, Unit](graphML.toString)
    // #simple-graphml
  }

  {
    // #custom-node-graphml
    import com.flowtick.graphs.Graph
    import com.flowtick.graphs.graphml._
    import com.flowtick.graphs.graphml.generic._
    import scala.xml.NodeSeq

    final case class MyNode(value: Int)

    val customGraph: Graph[GraphMLEdge[Unit], GraphMLNode[MyNode], GraphMLGraph[Unit]] =
      GraphML.fromEdges(Set(
        ml(MyNode(1), id = Some("one")) --> ml(MyNode(2), id = Some("two"))))

    val xml: NodeSeq = ToGraphML[Unit, MyNode, Unit](customGraph)
    val loaded = FromGraphML[Unit, MyNode, Unit](xml.toString)
    // #custom-node-graphml
  }

  {
    import com.flowtick.graphs.Graph
    import com.flowtick.graphs.defaults._
    import com.flowtick.graphs.graphml._
    import com.flowtick.graphs.graphml.generic._

    val graph = Graph.fromEdges[Unit, String](Set(n("A") --> n("B")))

    val xml = graph.asGraphML().xml
    val loaded = FromGraphML[Unit, String, Unit](xml.toString)
  }
}
