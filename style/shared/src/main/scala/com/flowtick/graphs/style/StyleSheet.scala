package com.flowtick.graphs.style

final case class NodeStyles(classes: Map[String, NodeShape] = Map.empty,
                            overrides: Map[String, NodeShape] = Map.empty) {
  private def mergeNodeShape(left: NodeShape, right: NodeShape): NodeShape =
    left.copy(
      fill = right.fill.orElse(left.fill),
      labelStyle = right.labelStyle.orElse(left.labelStyle),
      shapeType = right.shapeType.orElse(left.shapeType),
      borderStyle = right.borderStyle.orElse(left.borderStyle),
      image = right.image.orElse(left.image),
      svgContent = right.svgContent.orElse(left.svgContent),
    )

  def getNodeStyle(id: Option[String], classList: List[String] = List.empty)(defaultNode: Option[NodeShape]): NodeShape =
    (defaultNode ++ classList.flatMap(classes.get) ++ id.flatMap(overrides.get)).foldLeft(NodeShape())(mergeNodeShape)

  def withOverride(id: String, nodeShape: Option[NodeShape] => Option[NodeShape]): NodeStyles =
    copy(overrides = overrides.updatedWith(id)(nodeShape))
}

final case class EdgeStyles(classes: Map[String, EdgeShape] = Map.empty,
                            overrides: Map[String, EdgeShape] = Map.empty) {
  private def mergeEdgeShape(left: EdgeShape, right: EdgeShape): EdgeShape =
    left.copy(
      labelStyle = right.labelStyle.orElse(left.labelStyle),
      edgeStyle = right.edgeStyle.orElse(left.edgeStyle)
    )

  def getEdgeStyle(id: Option[String], classList: List[String] = List.empty)(defaultEdge: Option[EdgeShape]): EdgeShape =
    (defaultEdge ++ classList.flatMap(classes.get) ++ id.flatMap(overrides.get)).foldLeft(EdgeShape())(mergeEdgeShape)

  def withOverride(id: String, edgeShape: Option[EdgeShape] => Option[EdgeShape]): EdgeStyles =
    copy(overrides = overrides.updatedWith(id)(edgeShape))
}

final case class StyleSheet(defaultNode: Option[NodeShape] = None,
                            defaultEdge: Option[EdgeShape] = None,
                            nodes: Option[NodeStyles] = None,
                            edges: Option[EdgeStyles] = None,
                            images: Map[String, ImageSpec] = Map.empty) {
  def withImage(id: String, imageSpec: ImageSpec): StyleSheet =
    copy(images = images + (id -> imageSpec))

  def updateNodeStyle(id: String, nodeShape: Option[NodeShape] => Option[NodeShape]): StyleSheet =
    copy(nodes = Some(nodes.getOrElse(NodeStyles()).withOverride(id, nodeShape)))

  def updateEdgeStyle(id: String, edgeShape: Option[EdgeShape] => Option[EdgeShape]): StyleSheet =
    copy(edges = Some(edges.getOrElse(EdgeStyles()).withOverride(id, edgeShape)))

  def getEdgeStyle(id: Option[String], classList: List[String] = List.empty): EdgeShape = {
    edges.map(_.getEdgeStyle(id, classList)(defaultEdge)).getOrElse(EdgeShape())
  }

  def getNodeStyle(id: Option[String], classList: List[String] = List.empty): NodeShape = {
    nodes.map(_.getNodeStyle(id, classList)(defaultNode)).getOrElse(NodeShape())
  }
}

object StyleSheet {
  val empty: StyleSheet = StyleSheet()
}
