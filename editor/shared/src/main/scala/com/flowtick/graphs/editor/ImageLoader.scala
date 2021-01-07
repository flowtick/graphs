package com.flowtick.graphs.editor

import cats.effect.IO
import com.flowtick.graphs.style.ImageSpec

trait ImageLoader[T] {
  def registerImage(ref: String,
                    imageSpec: ImageSpec): IO[T]
  def getImage(ref: String): Option[T]
}

object ImageLoader {
  def unescapeXml(xml: String): String = xml
    .replaceAll("&gt;", ">")
    .replaceAll("&lt;", "<")
    .replaceAll("&#13;", "")
}