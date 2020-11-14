package com.flowtick.graphs.editor

import com.flowtick.graphs.style.EdgeShape

final case class Stencil(id: String,
                         title: String,
                         previewImageRef: Option[String] = None,
                         schemaRef: Option[String] = None)

final case class Connector(id: String,
                           title: String,
                           previewImageRef: Option[String] = None,
                           schemaRef: Option[String] = None,
                           shape: Option[EdgeShape] = None)

final case class ConnectorGroup(title: String, items: List[Connector])
final case class StencilGroup(title: String, items: List[Stencil])

final case class Palette(stencils: List[StencilGroup] = List.empty,
                         connectors: List[ConnectorGroup] = List.empty)

object Palette {
  lazy val defaultPalette = Palette(
    stencils = List(
      StencilGroup("Default Shapes", List(
        Stencil("white_rect", "White Rectangle",
          previewImageRef = Some("white_rect")
        )
      ))))
}
