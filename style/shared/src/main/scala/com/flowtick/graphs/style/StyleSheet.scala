package com.flowtick.graphs.style

trait StyleMonoid[S] {
  def empty: S
  def combine(left: S, right: S): S
}

object StyleMonoid {
  implicit val nodeStyles = new StyleMonoid[NodeShape] {
    override def empty: NodeShape = NodeShape()

    override def combine(left: NodeShape, right: NodeShape): NodeShape =
      left.copy(
        fill = right.fill.orElse(left.fill),
        labelStyle = right.labelStyle.orElse(left.labelStyle),
        shapeType = right.shapeType.orElse(left.shapeType),
        borderStyle = right.borderStyle.orElse(left.borderStyle),
        image = right.image.orElse(left.image),
        svgContent = right.svgContent.orElse(left.svgContent)
      )
  }

  implicit val edgeStyles = new StyleMonoid[EdgeShape] {
    override def empty: EdgeShape = EdgeShape()
    override def combine(left: EdgeShape, right: EdgeShape): EdgeShape =
      left.copy(
        labelStyle = right.labelStyle.orElse(left.labelStyle),
        edgeStyle = right.edgeStyle.orElse(left.edgeStyle),
        arrows = right.arrows.orElse(left.arrows)
      )
  }
}

final case class Styles[S](
    classes: Option[Map[String, S]] = None,
    overrides: Option[Map[String, S]] = None
)(implicit monoid: StyleMonoid[S]) {
  def getStyle(id: Option[String], classList: List[String] = List.empty)(
      default: Option[S]
  ): S =
    (default ++
      classList.flatMap(classes.getOrElse(Map.empty).get) ++
      id.flatMap(overrides.getOrElse(Map.empty).get))
      .foldLeft(monoid.empty)(monoid.combine)

  def withOverride(id: String, update: Option[S] => Option[S]): Styles[S] =
    copy(overrides =
      Some(
        update(overrides.getOrElse(Map.empty).get(id))
          .map(style => overrides.getOrElse(Map.empty) + (id -> style))
          .getOrElse(overrides.getOrElse(Map.empty))
      )
    )

  def ++(styles: Styles[S]): Styles[S] =
    copy(
      overrides = Some((styles.overrides ++ overrides).reduce(_ ++ _)),
      classes = Some((styles.classes ++ classes).reduce(_ ++ _))
    )
}

trait StyleSheetLike {
  def id: Option[String]

  def styleSheets: List[StyleSheet]

  def nodeStyles: Styles[NodeShape]
  def edgeStyles: Styles[EdgeShape]

  def getNodeStyle(
      id: Option[String],
      classList: List[String] = List.empty
  ): Option[NodeShape]
  def getEdgeStyle(
      id: Option[String],
      classList: List[String] = List.empty
  ): Option[EdgeShape]

  def requireNodeStyle(
      id: Option[String],
      classList: List[String] = List.empty
  ): NodeShape =
    getNodeStyle(id, classList).getOrElse(NodeShape())

  def requireEdgeStyle(
      id: Option[String],
      classList: List[String] = List.empty
  ): EdgeShape =
    getEdgeStyle(id, classList).getOrElse(EdgeShape())

  def images: Map[String, ImageSpec]

  def updateNodeStyle(
      id: String,
      nodeShape: Option[NodeShape] => Option[NodeShape]
  ): StyleSheetLike
  def updateEdgeStyle(
      id: String,
      edgeShape: Option[EdgeShape] => Option[EdgeShape]
  ): StyleSheetLike

  def withImage(id: String, imageSpec: ImageSpec): StyleSheetLike
  def withNodeDefault(nodeDefault: NodeShape): StyleSheetLike
  def withEdgeDefault(edgeDefault: EdgeShape): StyleSheetLike

  def merge(other: List[StyleSheet]): StyleSheetLike
}

final case class StyleSheet(
    id: Option[String] = None,
    nodes: Option[Styles[NodeShape]] = None,
    edges: Option[Styles[EdgeShape]] = None,
    images: Map[String, ImageSpec] = Map.empty,
    nodeDefault: Option[NodeShape] = None,
    edgeDefault: Option[EdgeShape] = None
) extends StyleSheetLike {
  override lazy val nodeStyles: Styles[NodeShape] =
    nodes.getOrElse(Styles[NodeShape]()(StyleMonoid.nodeStyles))
  override lazy val edgeStyles: Styles[EdgeShape] =
    edges.getOrElse(Styles[EdgeShape]()(StyleMonoid.edgeStyles))

  def withImage(id: String, imageSpec: ImageSpec): StyleSheet =
    copy(images = images + (id -> imageSpec))

  def withNodeDefault(nodeDefault: NodeShape): StyleSheet =
    copy(nodeDefault = Some(nodeDefault))

  def withEdgeDefault(edgeDefault: EdgeShape): StyleSheet =
    copy(edgeDefault = Some(edgeDefault))

  def updateNodeStyle(
      id: String,
      nodeShape: Option[NodeShape] => Option[NodeShape]
  ): StyleSheet =
    copy(nodes = Some(nodeStyles.withOverride(id, nodeShape)))

  def updateEdgeStyle(
      id: String,
      edgeShape: Option[EdgeShape] => Option[EdgeShape]
  ): StyleSheet =
    copy(edges = Some(edgeStyles.withOverride(id, edgeShape)))

  def getEdgeStyle(
      id: Option[String],
      classList: List[String] = List.empty
  ): Option[EdgeShape] = {
    Some(edgeStyles.getStyle(id, classList)(edgeDefault))
  }

  def getNodeStyle(
      id: Option[String],
      classList: List[String] = List.empty
  ): Option[NodeShape] = {
    Some(nodeStyles.getStyle(id, classList)(nodeDefault))
  }

  override def styleSheets: List[StyleSheet] = List(this)

  override def merge(other: List[StyleSheet]): StyleSheetLike =
    StyleSheets(List(this) ++ other.filterNot(_.id == id))
}

final case class StyleSheets(styleSheets: List[StyleSheet] = List.empty) extends StyleSheetLike {
  override lazy val nodeStyles: Styles[NodeShape] =
    styleSheets.view.map(_.nodeStyles).reduce(_ ++ _)
  override lazy val edgeStyles: Styles[EdgeShape] =
    styleSheets.view.map(_.edgeStyles).reduce(_ ++ _)

  override def id: Option[String] = None

  override def getNodeStyle(
      id: Option[String],
      classList: List[String]
  ): Option[NodeShape] =
    styleSheets.map(_.getNodeStyle(id, classList)).find(_.isDefined).flatten

  override def getEdgeStyle(
      id: Option[String],
      classList: List[String]
  ): Option[EdgeShape] =
    styleSheets.map(_.getEdgeStyle(id, classList)).find(_.isDefined).flatten

  override lazy val images: Map[String, ImageSpec] =
    styleSheets.map(_.images).foldLeft(Map.empty[String, ImageSpec])(_ ++ _)

  private def updateFirstSheet(
      filter: StyleSheet => Boolean
  )(update: StyleSheet => StyleSheet): StyleSheets = {
    val sheet = styleSheets
      .find(filter)
      .orElse(styleSheets.headOption)
      .getOrElse(StyleSheet(None))

    copy(styleSheets = styleSheets.filterNot(_.id == sheet.id) :+ update(sheet))
  }

  override def updateNodeStyle(
      id: String,
      nodeShape: Option[NodeShape] => Option[NodeShape]
  ): StyleSheetLike =
    updateFirstSheet(_.nodeStyles.overrides.exists(_.contains(id)))(
      _.updateNodeStyle(id, nodeShape)
    )

  override def updateEdgeStyle(
      id: String,
      edgeShape: Option[EdgeShape] => Option[EdgeShape]
  ): StyleSheetLike =
    updateFirstSheet(_.edgeStyles.overrides.exists(_.contains(id)))(
      _.updateEdgeStyle(id, edgeShape)
    )

  override def withImage(id: String, imageSpec: ImageSpec): StyleSheetLike =
    updateFirstSheet(_.images.contains(id))(_.withImage(id, imageSpec))

  override def merge(other: List[StyleSheet]): StyleSheetLike =
    copy(
      styleSheets ++ other.filterNot(
        _.id.exists(styleSheets.flatMap(_.id).contains)
      )
    )

  override def withNodeDefault(nodeDefault: NodeShape): StyleSheetLike =
    updateFirstSheet(_ => false)(_.withNodeDefault(nodeDefault))

  override def withEdgeDefault(edgeDefault: EdgeShape): StyleSheetLike =
    updateFirstSheet(_ => false)(_.withEdgeDefault(edgeDefault))
}

trait StyleRef[T] {
  def id(element: T): Option[String]
  def classList(element: T): List[String]
}
