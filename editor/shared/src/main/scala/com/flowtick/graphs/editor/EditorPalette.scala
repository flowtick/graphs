package com.flowtick.graphs.editor

import com.flowtick.graphs.json.schema.Schema
import com.flowtick.graphs.style.{ImageSpec, StyleSheet}

/** @param id
  *   the id of the stencil, this will be used as a reference to the style sheet
  * @param title
  *   title to show
  * @param image
  *   an image spec to be used in the palette, this is not referencing the stylesheet images because
  *   the stencil style might actually not be an image, making a preview hard to reproduce
  * @param schemaRef
  *   reference to the schema definition, not the full path, only the fragment
  */
final case class Stencil(
    id: String,
    title: String,
    image: Option[ImageSpec] = None,
    schemaRef: Option[String] = None
)

final case class Connector(
    id: String,
    title: String,
    image: Option[ImageSpec] = None,
    schemaRef: Option[String] = None
)

final case class ConnectorGroup(title: String, items: List[Connector])
final case class StencilGroup(title: String, items: List[Stencil])

final case class Palette(
    stencils: List[StencilGroup] = List.empty,
    connectors: List[ConnectorGroup] = List.empty,
    styleSheet: StyleSheet,
    schema: Schema[EditorSchemaHints]
)

trait EditorPaletteLike {
  private lazy val stencilById: Map[String, Stencil] =
    palettes
      .flatMap(_.stencils)
      .flatMap(_.items)
      .map(stencil => (stencil.id, stencil))
      .toMap

  private lazy val connectorsById: Map[String, Connector] =
    palettes
      .flatMap(_.connectors)
      .flatMap(_.items)
      .map(connector => (connector.id, connector))
      .toMap

  def palettes: List[Palette]

  def stencils: Iterable[Stencil] = stencilById.values

  def connectors: Iterable[Connector] = connectorsById.values

  def findStencil(id: String): Option[Stencil] = stencilById.get(id)

  def findConnector(id: String): Option[Connector] = connectorsById.get(id)

  def stencilGroups: Iterable[StencilGroup] = palettes.view.flatMap(_.stencils)

  def connectorGroups: Iterable[ConnectorGroup] =
    palettes.view.flatMap(_.connectors)
}

final case class EditorPalettes(palettes: List[Palette]) extends EditorPaletteLike
