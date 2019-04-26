package com.flowtick.graphs.graphml

import cats.data.Validated.Valid
import org.scalatest.{ FlatSpec, Matchers }

case class SomeNodeValue(one: String, two: String)

class GraphMLNodeDatatypeSpec extends FlatSpec with Matchers {
  it should "serialize a generic GraphML node" in {
    val fooDataType = implicitly[Datatype[GraphMLNode[SomeNodeValue]]]

    val serialized = fooDataType.serialize(GraphMLNode(id = "test", value = SomeNodeValue("foo", "bar")))
    val deserialized = fooDataType.deserialize(serialized)

    println(serialized)
    println(deserialized)

    serialized.toString should be(<value>bar</value><value>42.0</value>.mkString(""))
    deserialized.map(_.value) should be(Valid(SomeNodeValue("foo", "bar")))
  }
}
