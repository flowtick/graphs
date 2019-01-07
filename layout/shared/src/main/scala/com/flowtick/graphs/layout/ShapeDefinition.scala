package com.flowtick.graphs.layout

final case class ShapeDefinition(
  width: Int = 30,
  height: Int = 30,
  shapeType: String = "rectangle",
  color: String = "#FFFFFF",
  rounded: Boolean = false)
