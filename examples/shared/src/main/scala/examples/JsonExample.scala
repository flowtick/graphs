package examples

trait JsonExample {
  {
    // #json_simple
    import com.flowtick.graphs._
    import com.flowtick.graphs.defaults._
    import com.flowtick.graphs.json.format.default._
    import io.circe._
    import io.circe.syntax._
    import io.circe.parser._

    val graph: Graph[Unit, String] = Graph.fromEdges(
      Set(
        "A" --> "D",
        "A" --> "C",
        "A" --> "B",
        "B" --> "E",
        "B" --> "F",
        "B" --> "G",
        "E" --> "H"
      )
    )

    val json: Json = graph.asJson
    val parsed: Either[Error, Graph[Unit, String]] = decode[Graph[Unit, String]](json.noSpaces)

    require(parsed == Right(graph))
    // #json_simple
    println(json.noSpaces)
  }

  {
    // #json_custom
    import com.flowtick.graphs._
    import com.flowtick.graphs.defaults._
    import com.flowtick.graphs.json.format.default._
    import io.circe._
    import io.circe.generic.auto._
    import io.circe.syntax._
    import io.circe.parser._

    case class MyNode(id: String, value: Double)

    implicit val myNodeId: Identifiable[MyNode] = (value: MyNode) => value.id

    val graph: Graph[Unit, MyNode] = Graph.fromEdges(
      Set(
        MyNode("1", 42) --> MyNode("2", 43)
      )
    )

    val json: Json = graph.asJson
    val parsed = decode[Graph[Unit, MyNode]](json.noSpaces)

    require(parsed == Right(graph))
    // #json_custom
  }
}
