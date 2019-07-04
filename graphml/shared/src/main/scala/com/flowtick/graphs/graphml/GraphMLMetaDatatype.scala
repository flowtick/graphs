package com.flowtick.graphs.graphml
import cats.data.ValidatedNel

import scala.xml.NodeSeq

class GraphMLMetaDatatype[M](implicit metaDatatype: Datatype[M]) extends Datatype[GraphMLGraph[M]] {
  override def deserialize(from: NodeSeq, graphKeys: collection.Map[String, GraphMLKey]): ValidatedNel[Throwable, GraphMLGraph[M]] =
    metaDatatype.deserialize(from, graphKeys).map(meta => GraphMLGraph(meta, None, graphKeys.values.toSeq))

  override def serialize(graph: GraphMLGraph[M]): NodeSeq = metaDatatype.serialize(graph.meta)
}
