package com.flowtick.graphs.style

final case class NodeStyles(classes: Option[Map[String, NodeShape]] = None,
                            overrides: Option[Map[String, NodeShape]] = None) {
  private def mergeNodeShape(left: NodeShape, right: NodeShape): NodeShape =
    left.copy(
      fill = right.fill.orElse(left.fill),
      labelStyle = right.labelStyle.orElse(left.labelStyle),
      shapeType = right.shapeType.orElse(left.shapeType),
      borderStyle = right.borderStyle.orElse(left.borderStyle),
      image = right.image.orElse(left.image),
      svgContent = right.svgContent.orElse(left.svgContent),
    )

  def getNodeStyle(id: Option[String],
                   classList: List[String] = List.empty)(defaultNode: Option[NodeShape]): NodeShape =
    (defaultNode ++
      classList.flatMap(classes.getOrElse(Map.empty).get) ++
      id.flatMap(overrides.getOrElse(Map.empty).get)).foldLeft(NodeShape())(mergeNodeShape)

  def withOverride(id: String, nodeShape: Option[NodeShape] => Option[NodeShape]): NodeStyles =
    copy(overrides = Some(
      nodeShape(overrides.getOrElse(Map.empty).get(id))
        .map(newShape => overrides.getOrElse(Map.empty) + (id -> newShape))
        .getOrElse(overrides.getOrElse(Map.empty))
    ))
}

final case class EdgeStyles(classes: Option[Map[String, EdgeShape]] = Some(Map.empty),
                            overrides: Option[Map[String, EdgeShape]] = Some(Map.empty)) {
  private def mergeEdgeShape(left: EdgeShape, right: EdgeShape): EdgeShape =
    left.copy(
      labelStyle = right.labelStyle.orElse(left.labelStyle),
      edgeStyle = right.edgeStyle.orElse(left.edgeStyle),
      arrows = right.arrows.orElse(left.arrows)
    )

  def getEdgeStyle(id: Option[String],
                   classList: List[String] = List.empty)(defaultEdge: Option[EdgeShape]): EdgeShape =
    (defaultEdge ++
      classList.flatMap(classes.getOrElse(Map.empty).get) ++
      id.flatMap(overrides.getOrElse(Map.empty).get)).foldLeft(EdgeShape())(mergeEdgeShape)

  def withOverride(id: String, edgeShape: Option[EdgeShape] => Option[EdgeShape]): EdgeStyles =
    copy(overrides = Some(
      edgeShape(overrides.getOrElse(Map.empty).get(id))
        .map(newShape => overrides.getOrElse(Map.empty) + (id -> newShape))
        .getOrElse(overrides.getOrElse(Map.empty))
    ))
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
