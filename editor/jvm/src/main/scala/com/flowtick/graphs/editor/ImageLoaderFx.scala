package com.flowtick.graphs.editor

import cats.effect.IO
import cats.implicits._
import com.flowtick.graphs.editor.view.SVGTranscoder
import com.flowtick.graphs.style.ImageSpec
import scalafx.scene.image.Image

import java.io.{ByteArrayInputStream, FileOutputStream, InputStream}
import java.util.Base64
import scala.collection.mutable

object ImageLoaderFx extends ImageLoader[Image] {
  val images: mutable.Map[String, Image] =
    scala.collection.mutable.Map[String, Image]()

  override def getImage(ref: String): Option[Image] = images.get(ref)

  val defaultScale = 150.0
  val smooth = true
  val preserveRatio = true

  override def registerImage(ref: String, imageSpec: ImageSpec): IO[Image] = {
    images.get(ref) match {
      case Some(existing) => IO.pure(existing)
      case None =>
        val scaleWidth = imageSpec.width
        val scaleHeight = imageSpec.height

        val loaded = imageSpec.imageType match {
          case "svg" =>
            for {
              svgXml <- IO(
                ImageLoader.unescapeXml(imageSpec.data).getBytes("UTF-8")
              )
              rendered <- renderSvg(
                ref,
                new ByteArrayInputStream(svgXml),
                scaleWidth,
                scaleHeight
              )
            } yield rendered

          case "dataUrl" =>
            imageSpec.data.split(",").toList match {
              case "data:image/png;base64" :: base64Data :: Nil =>
                scaledImage(
                  inputStreamFromBase64(base64Data),
                  scaleWidth,
                  scaleHeight
                )

              case "data:image/svg+xml;base64" :: base64Data :: Nil =>
                renderSvg(
                  ref,
                  inputStreamFromBase64(base64Data),
                  scaleWidth,
                  scaleHeight
                )

              case other =>
                IO.raiseError(
                  new IllegalArgumentException(
                    s"unsupported data url format: $other for $ref ($imageSpec)"
                  )
                )
            }
          case "url" =>
            val loadImage =
              IO(scala.io.Source.fromURL(imageSpec.data)).bracket { imageSource =>
                for {
                  data <- IO(
                    new ByteArrayInputStream(
                      imageSource.mkString.getBytes("UTF-8")
                    )
                  )
                  image <-
                    if (imageSpec.data.toLowerCase.endsWith(".svg")) {
                      renderSvg(ref, data, scaleWidth, scaleHeight)
                    } else
                      IO(
                        new Image(
                          data,
                          scaleWidth.getOrElse(defaultScale),
                          scaleHeight.getOrElse(defaultScale),
                          preserveRatio,
                          smooth
                        )
                      )
                } yield image
              }(source => IO(source.close()))

            loadImage.attempt.flatMap {
              case Right(image) if image.error.value =>
                IO.raiseError(
                  new RuntimeException(
                    s"error in image loader for $imageSpec",
                    image.exception.value
                  )
                )
              case Right(image) => IO.pure(image)
              case Left(error) =>
                IO.raiseError(
                  new RuntimeException(s"unable to load $imageSpec", error)
                )
            }
          case other =>
            IO.raiseError(
              new IllegalArgumentException(s"unsupported image type: $other")
            )
        }

        loaded.flatTap { image =>
          IO(images.put(ref, image))
        }
    }
  }

  private def inputStreamFromBase64(base64: String): InputStream = {
    new ByteArrayInputStream(Base64.getDecoder.decode(base64))
  }

  private def scaledImage(
      inputStream: InputStream,
      scaleWidth: Option[Double],
      scaleHeight: Option[Double]
  ): IO[Image] = IO {
    val image = new Image(
      inputStream,
      scaleWidth.getOrElse(defaultScale.toDouble),
      scaleHeight.getOrElse(defaultScale.toDouble),
      preserveRatio,
      smooth
    )
    if (image.isError) {
      throw new RuntimeException(s"unable to load image", image.exception.value)
    } else image
  }

  def renderSvg(
      id: String,
      input: InputStream,
      scaleWidth: Option[Double],
      scaleHeight: Option[Double]
  ): IO[Image] =
    SVGTranscoder
      .transcodeSvgToPng(input, scaleWidth, scaleHeight)
      .flatMap { bytes =>
        val fileOut = new FileOutputStream(s"target/$id.png")
        fileOut.write(bytes)
        fileOut.flush()
        fileOut.close()

        scaledImage(new ByteArrayInputStream(bytes), scaleWidth, scaleHeight)
      }

}
