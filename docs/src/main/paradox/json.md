## JSON support

`graphs` supports the serialization of graphs as via [circe](https://github.com/circe/circe).

You need to add the `graphs-json` dependency to use it:

@@@vars
```scala
libraryDependencies += "com.flowtick" %% "graphs-json" % "$version$"
```
@@@

For the conversion you need to have an `Identifiable` instance for the node type:

#### Example: primitive types with defaults

@@snip [JsonExample.scala](../../../../examples/shared/src/main/scala/examples/JsonExample.scala){ #json_simple }

#### Example: providing the ID for a custom type

@@snip [JsonExample.scala](../../../../examples/shared/src/main/scala/examples/JsonExample.scala){ #json_custom }

Note that this example is also importing to option to treat unit as `null` in the JSON representation. 
