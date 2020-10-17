package lzw.streams

import java.io.ByteArrayOutputStream
import lzw.bytes.{LzwByteEncoder, Options}
import lzw.utils.VaryingOptionsRunnerSupport
import scala.annotation.tailrec
import scala.collection.mutable

object LzwDecoderOutputStreamRoundTripTestsRunner {

  def main(args: Array[String]): Unit =
    VaryingOptionsRunnerSupport.run(processCase)

  def processCase(options: Options, resets: Int): Unit = {
    val encodedBytes = {
      val encodedBytesBuffer = mutable.Buffer.empty[Byte]
      val encoder = new LzwByteEncoder(options)
      val decodedContentParts = VaryingOptionsRunnerSupport.decodedContentByResets(resets)
      for ((part, i) <- decodedContentParts.zipWithIndex) {
        if (i > 0) encodedBytesBuffer.appendAll(encoder.reset())
        val encodedBytes = encoder.encode(part)
        encodedBytesBuffer.appendAll(encodedBytes)
      }
      encodedBytesBuffer.appendAll(encoder.finish())
      encodedBytesBuffer.toArray
    }

    val contentOutputStream = new ByteArrayOutputStream()
    val decoderOutputStream = new LzwDecoderOutputStream(contentOutputStream, options)

    val parts = resets + 1
    val writeBufferSize = 1 + VaryingOptionsRunnerSupport.decodedContent.length / (3 * (parts + 1))
    val writeMethodOverrides = new WriteMethodOverrides(decoderOutputStream, writeBufferSize, encodedBytes)

    @tailrec
    def loop(writeMethodOverride: writeMethodOverrides.Override = writeMethodOverrides.singleByte): Unit = {
      if (writeMethodOverride.execute()) {
        loop(writeMethodOverride.next)
      }
    }

    loop()
    val decodedBytes = contentOutputStream.toByteArray

    val expectedDecodedBytes = VaryingOptionsRunnerSupport.decodedContent
    assert(decodedBytes `sameElements` expectedDecodedBytes, (decodedBytes.length, expectedDecodedBytes.length))
  }
}
