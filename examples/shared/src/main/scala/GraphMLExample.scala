trait GraphMLExample {
  {
    // #simple-graphml
    import com.flowtick.graphs.Graph
    import com.flowtick.graphs.defaults._
    import com.flowtick.graphs.defaults.label._
    import com.flowtick.graphs.graphml._
    import com.flowtick.graphs.graphml.generic._

    val simple: Graph[Unit, Unit, String] = Graph.fromEdges(Set(
      "A" --> "B",
      "B" --> "C",
      "D" --> "A"))

    val graphML = simple.asGraphML.xml
    val loaded = FromGraphML[Unit, Int, String](graphML.toString)
    // #simple-graphml
  }

  {
    // #custom-node-graphml
    import com.flowtick.graphs.Graph
    import com.flowtick.graphs.graphml._
    import com.flowtick.graphs.graphml.generic._
    import scala.xml.NodeSeq

    final case class MyNode(value: Int)

    val customGraph: Graph[GraphMLGraph[Unit], GraphMLEdge[Unit], GraphMLNode[MyNode]] =
      GraphML.fromEdges(Set(
        ml(MyNode(1), id = Some("one")) --> ml(MyNode(2), id = Some("two"))))

    val xml: NodeSeq = ToGraphML[Unit, Unit, MyNode](customGraph)
    val loaded = FromGraphML[Unit, Unit, MyNode](xml.toString)
    // #custom-node-graphml
  }
}
