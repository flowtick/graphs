package com.flowtick.graphs

import com.flowtick.graphs.graphml.{EdgeShape, Fill, NodeLabel, NodeShape, ShapeType}

final case class Stencil(id: String,
                         title: String,
                         shape: Option[NodeShape] = None,
                         imageDataRef: Option[String] = None,
                         previewImageRef: Option[String] = None,
                         schemaRef: Option[String] = None)

final case class Connector(id: String,
                           title: String,
                           previewImageRef: Option[String] = None,
                           schemaRef: Option[String] = None,
                           shape: Option[EdgeShape] = None)

final case class ConnectorGroup(title: String, items: List[Connector])
final case class StencilGroup(title: String, items: List[Stencil])

final case class ImageSpec(data: String, imageType: String)

final case class Palette(images: Map[String, ImageSpec] = Map.empty,
                         stencils: List[StencilGroup] = List.empty,
                         connectors: List[ConnectorGroup] = List.empty)

object Palette {
  lazy val defaultPalette = Palette(
    images = Map(
      "white_rect" -> ImageSpec(
        data = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAVsAAAD5CAYAAACJbJa8AAAACXBIWXMAAA7DAAAOwwHHb6hkAAAAGXRFWHRTb2Z0d2FyZQB3d3cuaW5rc2NhcGUub3Jnm+48GgAAA65JREFUeJzt2CFuwwAQRcFNVeibROa5P4hk7KuYp6hRc4C8SO4M+2zRA3uZmccA8FZfnz4A4D8QW4DA99+xLMus6/qpWwBOY9/3OY7juV9ie71e536/50cBnM3tdptt257bGwEgILYAAbEFCIgtQEBsAQJiCxAQW4CA2AIExBYgILYAAbEFCIgtQEBsAQJiCxAQW4CA2AIExBYgILYAAbEFCIgtQEBsAQJiCxAQW4CA2AIExBYgILYAAbEFCIgtQEBsAQJiCxAQW4CA2AIExBYgILYAAbEFCIgtQEBsAQJiCxAQW4CA2AIExBYgILYAAbEFCIgtQEBsAQJiCxAQW4CA2AIExBYgILYAAbEFCIgtQEBsAQJiCxAQW4CA2AIExBYgILYAAbEFCIgtQEBsAQJiCxAQW4CA2AIExBYgILYAAbEFCIgtQEBsAQJiCxAQW4CA2AIExBYgILYAAbEFCIgtQEBsAQJiCxAQW4CA2AIExBYgILYAAbEFCIgtQEBsAQJiCxAQW4CA2AIExBYgILYAAbEFCIgtQEBsAQJiCxAQW4CA2AIExBYgILYAAbEFCIgtQEBsAQJiCxAQW4CA2AIExBYgILYAAbEFCIgtQEBsAQJiCxAQW4CA2AIExBYgILYAAbEFCIgtQEBsAQJiCxAQW4CA2AIExBYgILYAAbEFCIgtQEBsAQJiCxAQW4CA2AIExBYgILYAAbEFCIgtQEBsAQJiCxAQW4CA2AIExBYgILYAAbEFCIgtQEBsAQJiCxAQW4CA2AIExBYgILYAAbEFCIgtQEBsAQJiCxAQW4CA2AIExBYgILYAAbEFCIgtQEBsAQJiCxAQW4CA2AIExBYgILYAAbEFCIgtQEBsAQJiCxAQW4CA2AIExBYgILYAAbEFCIgtQEBsAQJiCxAQW4CA2AIExBYgILYAAbEFCIgtQEBsAQJiCxAQW4CA2AIExBYgILYAAbEFCIgtQEBsAQJiCxAQW4CA2AIExBYgILYAAbEFCIgtQEBsAQJiCxAQW4CA2AIExBYgILYAAbEFCIgtQEBsAQJiCxAQW4CA2AIExBYgILYAAbEFCIgtQEBsAQJiCxAQW4CA2AIExBYgILYAAbEFCIgtQEBsAQJiCxAQW4CA2AIExBYgILYAAbEFCIgtQEBsAQJiCxAQW4CA2AIExBYgILYAAbEFCIgtQEBsAQJiCxAQW4CA2AIELjPz+B3Lssy6rh88B+Ac9n2f4zie+yW2ALyHNwJAQGwBAj93IRbHDiSbGgAAAABJRU5ErkJggg==",
        imageType = "dataUrl"
      )
    ),
    stencils = List(
      StencilGroup("shapes", List(
        Stencil("white_rect", "White Rectangle",
          shape = Some(NodeShape(
            shapeType = Some(ShapeType.Rectangle),
            label = Some(NodeLabel("White")),
            fill = Some(Fill(color = Some("#FFFFFF")))
          )),
          // TODO: replace with svg
          previewImageRef = Some("white_rect")
        )))))
}
