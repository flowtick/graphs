package com.flowtick.graphs.editor

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream}
import java.util.Base64

import cats.effect.IO
import cats.implicits._
import com.flowtick.graphs.style.ImageSpec
import org.apache.batik.transcoder.image.PNGTranscoder
import org.apache.batik.transcoder.{SVGAbstractTranscoder, TranscoderInput, TranscoderOutput}
import scalafx.scene.image.Image

import scala.collection.mutable

object ImageLoaderFx extends ImageLoader[Image] {
  val images: mutable.Map[String, Image] = scala.collection.mutable.Map[String, Image]()

  override def getImage(ref: String): Option[Image] = images.get(ref)

  val defaultScale = 250

  override def registerImage(ref: String,
                    imageSpec: ImageSpec,
                    scaleWidth: Option[Double] = None,
                    scaleHeight: Option[Double] = None): IO[Image] = {
    images.get(ref) match {
      case Some(existing) => IO.pure(existing)
      case None =>
        val loaded = imageSpec.imageType match {
          case "svg" =>
            renderSvg(ref, new ByteArrayInputStream(ImageLoader.unescapeXml(imageSpec.data).getBytes("UTF-8")), scaleWidth, scaleHeight)

          case "dataUrl" =>
            imageSpec.data.split(",").toList match {
              case "data:image/png;base64" :: base64Data :: Nil =>
                scaledImage(ref, inputStreamFromBase64(base64Data) , scaleWidth, scaleHeight)

              case "data:image/svg+xml;base64" :: base64Data :: Nil =>
                renderSvg(ref, inputStreamFromBase64(base64Data), scaleWidth, scaleHeight)

              case other => IO.raiseError(new IllegalArgumentException(s"unsupported data url format: $other for $ref ($imageSpec)"))
            }
          case "url" => IO(new Image(imageSpec.data, scaleWidth.getOrElse(defaultScale), scaleHeight.getOrElse(defaultScale), true, true, backgroundLoading = false))
            .attempt.flatMap {
              case Right(image) if image.error.value => IO.raiseError(new RuntimeException(s"error in image loader for $imageSpec", image.exception.value))
              case Right(image) => IO.pure(image)
              case Left(error) => IO.raiseError(new RuntimeException(s"unable to load $imageSpec", error))
            }
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

  private def renderSvg(id: String,
                        input: InputStream,
                        scaleWidth: Option[Double],
                        scaleHeight: Option[Double]): IO[Image] = IO(input).bracket { data =>
    val dpi = 100
    val pixelUnitToMillimeter = (2.54f / dpi) * 10
    val pngTranscoder = new PNGTranscoder()
    pngTranscoder.addTranscodingHint(SVGAbstractTranscoder.KEY_PIXEL_UNIT_TO_MILLIMETER, pixelUnitToMillimeter)

    val input = new TranscoderInput(data)
    val outputStream = new ByteArrayOutputStream()
    val output = new TranscoderOutput(outputStream)
    pngTranscoder.transcode(input, output)

    scaledImage(id, new ByteArrayInputStream(outputStream.toByteArray), scaleWidth, scaleHeight)
  }(data => IO(data.close()))

  private def scaledImage(id: String,
                  inputStream: InputStream,
                  scaleWidth: Option[Double],
                  scaleHeight: Option[Double]): IO[Image] = IO {
    val image = new Image(inputStream, scaleWidth.getOrElse(defaultScale.toDouble), scaleHeight.getOrElse(defaultScale.toDouble), true, false)
    if (image.isError) {
      throw new RuntimeException(s"unable to load image $id", image.exception.value)
    } else image
  }
}
