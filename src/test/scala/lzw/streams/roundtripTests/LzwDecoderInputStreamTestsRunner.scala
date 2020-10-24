package lzw.streams.roundtripTests

import java.io.ByteArrayInputStream
import lzw.bytes.{LzwByteEncoder, Options}
import lzw.streams.{LzwDecoderInputStream, ReadMethodOverrides}
import lzw.utils.VaryingOptionsRunnerSupport
import scala.annotation.tailrec
import scala.collection.mutable

object LzwDecoderInputStreamTestsRunner {

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

    val encodedBytesInputStream = new ByteArrayInputStream(encodedBytes)
    val decoderInputStream = new LzwDecoderInputStream(encodedBytesInputStream, options)

    val parts = resets + 1
    val Gap = 2
    val readBufferSize = 2 * Gap + 1 + VaryingOptionsRunnerSupport.decodedContent.length / (3 * (parts + 1))
    val decodedBytesBuffer = mutable.Buffer.empty[Byte]
    val readMethodOverrides = new ReadMethodOverrides(decoderInputStream, readBufferSize, bytes => decodedBytesBuffer.appendAll(bytes))

    @tailrec
    def loop(readMethodOverride: readMethodOverrides.Override = readMethodOverrides.singleByte): Unit =
      if (readMethodOverride.execute()) {
        loop(readMethodOverride.next)
      }

    loop()
    val decodedBytes = decodedBytesBuffer.toArray

    val expectedDecodedBytes = VaryingOptionsRunnerSupport.decodedContent
    assert(decodedBytes `sameElements` expectedDecodedBytes, (decodedBytes.length, expectedDecodedBytes.length))
  }
}
