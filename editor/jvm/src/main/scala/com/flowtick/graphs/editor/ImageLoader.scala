package com.flowtick.graphs.editor

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream}
import java.util.Base64

import cats.effect.IO
import cats.implicits._
import javax.imageio.ImageTranscoder
import org.apache.batik.transcoder.image.{ImageTranscoder, PNGTranscoder}
import org.apache.batik.transcoder.{SVGAbstractTranscoder, TranscoderInput, TranscoderOutput, TranscodingHints}
import scalafx.scene.image.Image

import scala.collection.mutable

object ImageLoader {
  val images: mutable.Map[String, Image] = scala.collection.mutable.Map[String, Image]()

  def getImage(ref: String): Option[Image] = images.get(ref)

  val defaultScale = 300

  def registerImage(ref: String,
                    imageSpec: ImageSpec,
                    scaleWidth: Option[Double] = None,
                    scaleHeight: Option[Double] = None): IO[Image] = {
    images.get(ref) match {
      case Some(existing) => IO.pure(existing)
      case None =>
        val loaded = imageSpec.imageType match {
          case "dataUrl" =>
            imageSpec.data.split(",").toList match {
              case "data:image/png;base64" :: base64Data :: Nil =>
                scaledImage(ref, inputStreamFromBase64(base64Data) , scaleWidth, scaleHeight)

              case "data:image/svg+xml;base64" :: base64Data :: Nil =>
                renderSvg(ref, base64Data, scaleWidth, scaleHeight)

              case other => IO.raiseError(new IllegalArgumentException(s"unsupported data url format: $other for $ref ($imageSpec)"))
            }
          case "url" => IO(new Image(imageSpec.data, scaleWidth.getOrElse(defaultScale), scaleHeight.getOrElse(defaultScale), true, true))
          case other => IO.raiseError(new IllegalArgumentException(s"unsupported image type: $other"))
        }

        loaded.flatTap { image =>
          IO(images.put(ref, image))
        }
    }
  }

  private def inputStreamFromBase64(base64: String): InputStream = {
    new ByteArrayInputStream(Base64.getDecoder.decode(base64))
  }

  private def renderSvg(id: String, base64Data: String,
                scaleWidth: Option[Double],
                scaleHeight: Option[Double]): IO[Image] = IO {
    val dpi = 100
    val pixelUnitToMillimeter = (2.54f / dpi) * 10
    val pngTranscoder = new PNGTranscoder()
    pngTranscoder.addTranscodingHint(SVGAbstractTranscoder.KEY_PIXEL_UNIT_TO_MILLIMETER, pixelUnitToMillimeter)

    val input = new TranscoderInput(new ByteArrayInputStream(Base64.getDecoder.decode(base64Data)))
    val outputStream = new ByteArrayOutputStream()
    val output = new TranscoderOutput(outputStream)
    pngTranscoder.transcode(input, output)
    new ByteArrayInputStream(outputStream.toByteArray)
  }.flatMap(scaledImage(id, _, scaleWidth, scaleHeight))

  private def scaledImage(id: String,
                  inputStream: InputStream,
                  scaleWidth: Option[Double],
                  scaleHeight: Option[Double]): IO[Image] = IO {
    val image = new Image(inputStream, scaleWidth.getOrElse(defaultScale), scaleHeight.getOrElse(defaultScale), true, false)
    if (image.isError) {
      throw new RuntimeException(s"unable to load $id", image.exception.value)
    } else image
  }
}
