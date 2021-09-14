package com.flowtick.graphs.editor.view

import cats.effect.IO
import org.apache.batik.transcoder.image.PNGTranscoder
import org.apache.batik.transcoder.{SVGAbstractTranscoder, TranscoderInput, TranscoderOutput}

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream}

object SVGTranscoder {
  def svgXmlToPng(
      svgXml: String,
      scaleWidth: Option[Double],
      scaleHeight: Option[Double]
  ): IO[Array[Byte]] =
    transcodeSvgToPng(
      new ByteArrayInputStream(svgXml.getBytes("UTF-8")),
      scaleWidth,
      scaleHeight
    )

  def transcodeSvgToPng(
      input: InputStream,
      scaleWidth: Option[Double],
      scaleHeight: Option[Double]
  ): IO[Array[Byte]] =
    IO(input).bracket(data =>
      IO {
        val dpi = 100
        val pixelUnitToMillimeter = (2.54f / dpi) * 10
        val pngTranscoder = new PNGTranscoder()
        pngTranscoder.addTranscodingHint(
          SVGAbstractTranscoder.KEY_PIXEL_UNIT_TO_MILLIMETER,
          pixelUnitToMillimeter
        )

        scaleWidth.foreach(width =>
          pngTranscoder.addTranscodingHint(
            SVGAbstractTranscoder.KEY_WIDTH,
            width.toFloat
          )
        )
        scaleHeight.foreach(height =>
          pngTranscoder.addTranscodingHint(
            SVGAbstractTranscoder.KEY_HEIGHT,
            height.toFloat
          )
        )

        val input = new TranscoderInput(data)
        val outputStream = new ByteArrayOutputStream()
        val output = new TranscoderOutput(outputStream)
        pngTranscoder.transcode(input, output)
        outputStream.toByteArray
      }
    )(data => IO(data.close()))

}
