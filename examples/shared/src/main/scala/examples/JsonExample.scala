package examples

trait JsonExample {
  {
    // #json_simple
    import com.flowtick.graphs._
    import com.flowtick.graphs.defaults._
    import com.flowtick.graphs.json._
    import com.flowtick.graphs.json.format.default._
    import io.circe
    import io.circe.Json

    val graph: Graph[Unit, String] = Graph.fromEdges(Set(
      "A" --> "D",
      "A" --> "C",
      "A" --> "B",
      "B" --> "E",
      "B" --> "F",
      "B" --> "G",
      "E" --> "H"))

    val json: Json = ToJson[Unit, Unit, String](graph)
    val parsed: Either[circe.Error, JsonGraph[Unit, Unit, String]] = FromJson[Unit, Unit, String](json.noSpaces)

    require(parsed.map(_.graph).contains(graph))
    // #json_simple
    println(json.noSpaces)
  }

  {
    // #json_custom
    import com.flowtick.graphs._
    import com.flowtick.graphs.defaults._
    import com.flowtick.graphs.json._
    import com.flowtick.graphs.json.format.default._
    import io.circe
    import io.circe.Json
    import io.circe.generic.auto._

    case class MyNode(id: String, value: Double)

    implicit val myNodeId: Identifiable[MyNode] = new Identifiable[MyNode] {
      override def apply(value: MyNode): String = value.id
    }

    val graph: Graph[Unit, MyNode] = Graph.fromEdges(Set(
      MyNode("1", 42) --> MyNode("2", 43)
    ))

    implicit val nodeId = Identifiable.identify[MyNode](_.id)

    val json: Json = ToJson[Unit, Unit, MyNode](graph)
    val parsed: Either[circe.Error, JsonGraph[Unit, Unit, MyNode]] = FromJson(json.noSpaces)

    require(parsed.map(_.graph).contains(graph))
    // #json_custom
  }
}
