import io.circe
import io.circe.Json

trait JsonExample {
  {
    // #json_simple
    import com.flowtick.graphs._
    import com.flowtick.graphs.defaults._
    import com.flowtick.graphs.json._
    import io.circe.generic.auto._

    val graph: Graph[Unit, Unit, String] = Graph.fromEdges(Set(
      "A" --> "D",
      "A" --> "C",
      "A" --> "B",
      "B" --> "E",
      "B" --> "F",
      "B" --> "G",
      "E" --> "H"))

    val json: Json = ToJson(graph)
    val parsed: Either[circe.Error, Graph[Unit, Unit, String]] = FromJson[Unit, Unit, String](json.noSpaces)

    require(parsed.contains(graph))
    // #json_simple
  }

  {
    // #json_custom
    import com.flowtick.graphs._
    import com.flowtick.graphs.defaults._
    import com.flowtick.graphs.json._
    import io.circe.generic.auto._

    import options.unitAsNull

    case class MyNode(id: String, value: Double)

    val graph: Graph[Unit, Unit, MyNode] = Graph.fromEdges(Set(
      MyNode("1", 42) --> MyNode("2", 43)
    ))

    implicit val nodeId = Identifiable.identify[MyNode, String](_.id)

    val json: Json = ToJson(graph)
    val parsed: Either[circe.Error, Graph[Unit, Unit, MyNode]] = FromJson(json.noSpaces)

    require(parsed.contains(graph))
    // #json_custom
  }
}
